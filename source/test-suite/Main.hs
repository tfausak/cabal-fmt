{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified CabalGild.Unstable.Class.MonadHandle as MonadHandle
import qualified CabalGild.Unstable.Class.MonadLog as MonadLog
import qualified CabalGild.Unstable.Class.MonadRead as MonadRead
import qualified CabalGild.Unstable.Class.MonadWalk as MonadWalk
import qualified CabalGild.Unstable.Class.MonadWrite as MonadWrite
import qualified CabalGild.Unstable.Exception.CheckFailure as CheckFailure
import qualified CabalGild.Unstable.Exception.DuplicateOption as DuplicateOption
import qualified CabalGild.Unstable.Exception.InvalidOption as InvalidOption
import qualified CabalGild.Unstable.Exception.SpecifiedOutputWithCheckMode as SpecifiedOutputWithCheckMode
import qualified CabalGild.Unstable.Exception.SpecifiedStdinWithFileInput as SpecifiedStdinWithFileInput
import qualified CabalGild.Unstable.Exception.UnexpectedArgument as UnexpectedArgument
import qualified CabalGild.Unstable.Exception.UnknownOption as UnknownOption
import qualified CabalGild.Unstable.Extra.String as String
import qualified CabalGild.Unstable.Main as Gild
import qualified CabalGild.Unstable.Type.Input as Input
import qualified CabalGild.Unstable.Type.Output as Output
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.RWS as RWST
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.Functor.Identity as Identity
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified GHC.Stack as Stack
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.FilePattern as FilePattern
import qualified System.IO.Temp as Temp
import qualified Test.Hspec as Hspec
import qualified Test.Main as TestMain

main :: IO ()
main = Hspec.hspec . Hspec.parallel . Hspec.describe "cabal-gild" $ do
  Hspec.it "shows the help" $ do
    let (a, s, w) = runGild ["--help"] [] (".", [])
    a `shouldBeFailure` Exit.ExitSuccess
    w `Hspec.shouldNotBe` []
    s `Hspec.shouldBe` Map.empty

  Hspec.it "shows the version" $ do
    let (a, s, w) = runGild ["--version"] [] (".", [])
    a `shouldBeFailure` Exit.ExitSuccess
    w `Hspec.shouldNotBe` []
    s `Hspec.shouldBe` Map.empty

  Hspec.it "fails with an unknown option" $ do
    expectException ["--unknown"] $
      UnknownOption.fromString "--unknown"

  Hspec.it "fails with an invalid option" $ do
    expectException ["--help=invalid"] $
      InvalidOption.fromString "option `--help' doesn't allow an argument"

  Hspec.it "fails with an unexpected argument" $ do
    expectException ["unexpected"] $
      UnexpectedArgument.fromString "unexpected"

  Hspec.it "fails when --crlf is given twice" $ do
    expectException ["--crlf=strict", "--crlf=lenient"] $
      DuplicateOption.DuplicateOption "crlf" "strict" "lenient"

  Hspec.it "does not fail when a flag is given twice with the same value" $ do
    let (a, s, w) =
          runGild
            ["--crlf=strict", "--crlf=strict"]
            [(Input.Stdin, String.toUtf8 "")]
            (".", [])
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.singleton Output.Stdout (String.toUtf8 "")

  Hspec.it "fails when a flag is repeatedly overriden" $ do
    expectException ["-ia", "-ib", "-i-"] $
      DuplicateOption.DuplicateOption "input" "a" "b"

  Hspec.it "fails when --input is given twice" $ do
    expectException
      ["--input=f", "--input=-"]
      $ DuplicateOption.DuplicateOption "input" "f" "-"

  Hspec.it "fails when --mode is given twice" $ do
    expectException
      ["--mode=check", "--mode=format"]
      $ DuplicateOption.DuplicateOption "mode" "check" "format"

  Hspec.it "fails when --output is given twice" $ do
    expectException
      ["--output=f", "--output=-"]
      $ DuplicateOption.DuplicateOption "output" "f" "-"

  Hspec.it "fails when --stdin is given twice" $ do
    expectException
      ["--stdin=f", "--stdin=g"]
      $ DuplicateOption.DuplicateOption "stdin" "f" "g"

  Hspec.it "reads from an input file" $ do
    let (a, s, w) =
          runGild
            ["--input", "input.cabal"]
            [(Input.File "input.cabal", String.toUtf8 "")]
            (".", [])
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.singleton Output.Stdout (String.toUtf8 "")

  Hspec.it "writes to an output file" $ do
    let (a, s, w) =
          runGild
            ["--output", "output.cabal"]
            [(Input.Stdin, String.toUtf8 "")]
            (".", [])
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.singleton (Output.File "output.cabal") (String.toUtf8 "")

  Hspec.it "succeeds when checking formatted input" $ do
    let (a, s, w) =
          runGild
            ["--mode", "check"]
            [(Input.Stdin, String.toUtf8 "pass: yes\n")]
            (".", [])
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.empty

  Hspec.it "fails when checking unformatted input" $ do
    let (a, s, w) =
          runGild
            ["--mode", "check"]
            [(Input.Stdin, String.toUtf8 "pass: no")]
            (".", [])
    a `shouldBeFailure` CheckFailure.CheckFailure
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.empty

  Hspec.it "succeeds when checking CRLF input leniently" $ do
    let (a, s, w) =
          runGild
            ["--mode", "check"]
            [(Input.Stdin, String.toUtf8 "pass: yes\r\n")]
            (".", [])
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.empty

  Hspec.it "fails when checking CRLF input strictly" $ do
    let (a, s, w) =
          runGild
            ["--crlf", "strict", "--mode", "check"]
            [(Input.Stdin, String.toUtf8 "pass: no\r\n")]
            (".", [])
    a `shouldBeFailure` CheckFailure.CheckFailure
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.empty

  Hspec.it "fails when --stdin is given with an input file" $ do
    let (a, s, w) =
          runGild
            ["--input", "f", "--stdin", "g"]
            []
            (".", [])
    a `shouldBeFailure` SpecifiedStdinWithFileInput.SpecifiedStdinWithFileInput
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.empty

  Hspec.it "fails when --output is given with check mode" $ do
    let (a, s, w) =
          runGild
            ["--mode", "check", "--output", "-"]
            []
            (".", [])
    a `shouldBeFailure` SpecifiedOutputWithCheckMode.SpecifiedOutputWithCheckMode
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.empty

  Hspec.it "does not overwrite output when input is formatted" $ do
    let (a, s, w) =
          runGild
            ["--io", "io.cabal"]
            [(Input.File "io.cabal", String.toUtf8 "")]
            (".", [])
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.empty

  Hspec.it "writes to stdout when input is formatted" $ do
    let (a, s, w) =
          runGild
            ["--input", "p.cabal"]
            [(Input.File "p.cabal", String.toUtf8 "")]
            (".", [])
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.singleton Output.Stdout (String.toUtf8 "")

  Hspec.it "writes to output when input is formatted" $ do
    let (a, s, w) =
          runGild
            ["--input", "p.cabal", "--output", "q.cabal"]
            [(Input.File "p.cabal", String.toUtf8 "")]
            (".", [])
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.singleton (Output.File "q.cabal") (String.toUtf8 "")

  Hspec.it "writes to output when stdin is formatted" $ do
    let (a, s, w) =
          runGild
            ["--output", "q.cabal"]
            [(Input.Stdin, String.toUtf8 "")]
            (".", [])
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.singleton (Output.File "q.cabal") (String.toUtf8 "")

  Hspec.it "sets input and output simultaneously" $ do
    let (a, s, w) =
          runGild
            ["--io", "io.cabal"]
            [(Input.File "io.cabal", String.toUtf8 "f:a")]
            (".", [])
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.singleton (Output.File "io.cabal") (String.toUtf8 "f: a\n")

  Hspec.it "does not overwrite CRLF file when lenient" $ do
    let (a, s, w) =
          runGild
            ["--io", "p.cabal"]
            [(Input.File "p.cabal", String.toUtf8 "s\r\n")]
            (".", [])
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.empty

  Hspec.it "overwrites CRLF file when strict" $ do
    let (a, s, w) =
          runGild
            ["--crlf", "strict", "--io", "p.cabal"]
            [(Input.File "p.cabal", String.toUtf8 "s\r\n")]
            (".", [])
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.singleton (Output.File "p.cabal") (String.toUtf8 "s\n")

  Hspec.it "uses --stdin for discovery" $ do
    let (a, s, w) =
          runGild
            ["--stdin", "d/p.cabal"]
            [(Input.Stdin, String.toUtf8 "library\n -- cabal-gild: discover\n exposed-modules:")]
            ("d", [["M.hs"]])
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.singleton Output.Stdout (String.toUtf8 "library\n  -- cabal-gild: discover\n  exposed-modules: M\n")

  Hspec.it "succeeds with empty input" $ do
    expectGilded
      ""
      ""

  Hspec.it "removes extra blank space" $ do
    expectGilded
      "\t\r\n \r\n"
      ""

  Hspec.it "formats a comment" $ do
    expectGilded
      "-- c"
      "-- c\n"

  Hspec.it "keeps a blank comment" $ do
    expectGilded
      "--"
      "--\n"

  Hspec.it "formats multiple comments" $ do
    expectGilded
      "-- c\n-- d"
      "-- c\n-- d\n"

  Hspec.it "removes blank lines between comments" $ do
    expectGilded
      "-- c\n\n-- d"
      "-- c\n-- d\n"

  Hspec.it "does not require a space after the comment start" $ do
    expectGilded
      "--c"
      "--c\n"

  Hspec.it "leaves leading blank space in comments" $ do
    expectGilded
      "--\t c"
      "--\t c\n"

  Hspec.it "trims trailing blank space from comments" $ do
    expectGilded
      "-- c\t \n"
      "-- c\n"

  Hspec.it "normalizes Windows line endings in comments" $ do
    expectGilded
      "-- c\r\n"
      "-- c\n"

  Hspec.it "formats a field without a value" $ do
    expectGilded
      "f:"
      "f:\n"

  Hspec.it "formats a field with a value" $ do
    expectGilded
      "f: 1"
      "f: 1\n"

  Hspec.it "does not require spaces before a field's value" $ do
    expectGilded
      "f:1"
      "f: 1\n"

  Hspec.it "allows spaces after a field's name" $ do
    expectGilded
      "f :1"
      "f: 1\n"

  Hspec.it "formats multiple fields" $ do
    expectGilded
      "f: 1\ng: 2"
      "f: 1\ng: 2\n"

  Hspec.it "formats a field with hung values" $ do
    expectGilded
      "f: 1\n 2"
      "f:\n  1\n  2\n"

  Hspec.it "formats a field with nested values" $ do
    expectGilded
      "f:\n 1\n 2"
      "f:\n  1\n  2\n"

  Hspec.it "puts a line after a field with multiple values" $ do
    expectGilded
      "f: 1\n 2\ng: 3"
      "f:\n  1\n  2\n\ng: 3\n"

  Hspec.it "does not pu a line before a field with multiple values" $ do
    expectGilded
      "f: 1\ng: 2\n 3"
      "f: 1\ng:\n  2\n  3\n"

  Hspec.it "converts field names to lowercase" $ do
    expectGilded
      "F: 1"
      "f: 1\n"

  Hspec.it "does not convert field values to lowercase" $ do
    expectGilded
      "f: A"
      "f: A\n"

  Hspec.it "normalizes spaces within field values" $ do
    expectGilded
      "f:\n a\n\n  b"
      "f:\n  a\n  b\n"

  Hspec.it "formats a section" $ do
    expectGilded
      "s"
      "s\n"

  Hspec.it "puts a line after a section" $ do
    expectGilded
      "s\nf: 1"
      "s\n\nf: 1\n"

  Hspec.it "puts a line before a section" $ do
    expectGilded
      "f: 1\ns"
      "f: 1\n\ns\n"

  Hspec.it "formats a section with an argument" $ do
    expectGilded
      "s a"
      "s a\n"

  Hspec.it "formats a section with multiple arguments" $ do
    expectGilded
      "s a b"
      "s a b\n"

  Hspec.it "formats a section with a quoted argument" $ do
    expectGilded
      "s \"a\""
      "s \"a\"\n"

  Hspec.it "formats a section with an operator argument" $ do
    expectGilded
      "s !"
      "s !\n"

  Hspec.it "converts section names to lowercase" $ do
    expectGilded
      "S"
      "s\n"

  Hspec.it "does not convert section arguments to lowercase" $ do
    expectGilded
      "s A"
      "s A\n"

  Hspec.it "formats nested sections" $ do
    -- Note that the nested section does not have an extra blank line before
    -- it.
    expectGilded
      "s\n t"
      "s\n  t\n"

  Hspec.it "formats a section with a field" $ do
    expectGilded
      "s\n f: 1"
      "s\n  f: 1\n"

  Hspec.it "formats a section with multiple fields" $ do
    expectGilded
      "s\n f: 1\n g: 2"
      "s\n  f: 1\n  g: 2\n"

  Hspec.it "correctly indents a section containing a field with multiple values" $ do
    expectGilded
      "s\n f:\n  1\n  2"
      "s\n  f:\n    1\n    2\n"

  Hspec.it "formats a comment before a field" $ do
    expectGilded
      "-- c\nf: 1"
      "-- c\nf: 1\n"

  Hspec.it "formats a comment after a field" $ do
    expectGilded
      "f: 1\n-- c"
      "f: 1\n-- c\n"

  Hspec.it "formats a comment before a field's value" $ do
    expectGilded
      "f:\n -- c\n 1"
      "f:\n  -- c\n  1\n"

  Hspec.it "formats a comment in a field's value" $ do
    expectGilded
      "f:\n 1\n -- c\n 2"
      "f:\n  -- c\n  1\n  2\n"

  Hspec.it "formats a comment after a field's value" $ do
    expectGilded
      "f:\n 1\n -- c"
      "f:\n  1\n\n-- c\n"

  Hspec.it "formats a comment after a field with multiple values" $ do
    expectGilded
      "f: 1\n 2\n-- c"
      "f:\n  1\n  2\n\n-- c\n"

  Hspec.it "formats a comment before a section" $ do
    expectGilded
      "-- c\ns"
      "-- c\ns\n"

  Hspec.it "formats a comment after a section" $ do
    expectGilded
      "s\n-- c"
      "s\n\n-- c\n"

  Hspec.it "correctly indents a comment in a section" $ do
    expectGilded
      "s\n -- c\n f: 1"
      "s\n  -- c\n  f: 1\n"

  Hspec.describe "description" $ do
    -- These tests apply to other "free text" fields as well. The description
    -- field is just a representative example.

    Hspec.describe "< 3.0" $ do
      -- These may look surprising, but they're the expected behavior.
      -- <https://github.com/haskell/cabal/issues/5938>

      Hspec.it "collapses extra blank lines" $ do
        expectGilded
          "description:\n 1\n\n 2"
          "description:\n  1\n  2\n"

      Hspec.it "trims leading blank space" $ do
        expectGilded
          "description:\n 1\n  2"
          "description:\n  1\n  2\n"

    Hspec.describe ">= 3.0" $ do
      Hspec.it "keeps one extra blank line" $ do
        expectGilded
          "cabal-version: 3.0\ndescription:\n 1\n\n 2"
          "cabal-version: 3.0\ndescription:\n  1\n\n  2\n"

      Hspec.it "keeps two extra blank lines" $ do
        -- Although Haddock will collapse these, Cabal itself keeps them.
        expectGilded
          "cabal-version: 3.0\ndescription:\n 1\n\n\n 2"
          "cabal-version: 3.0\ndescription:\n  1\n\n\n  2\n"

      Hspec.it "indents second line past first when nested" $ do
        expectGilded
          "cabal-version: 3.0\ndescription:\n 1\n  2"
          "cabal-version: 3.0\ndescription:\n  1\n   2\n"

      Hspec.it "indents first line past second when nested" $ do
        expectGilded
          "cabal-version: 3.0\ndescription:\n  1\n 2"
          "cabal-version: 3.0\ndescription:\n   1\n  2\n"

      Hspec.it "does not indent first line when hung" $ do
        expectGilded
          "cabal-version: 3.0\ndescription: 1\n 2"
          "cabal-version: 3.0\ndescription:\n  1\n  2\n"

      Hspec.it "indents second line past first when hung" $ do
        expectGilded
          "cabal-version: 3.0\ndescription: 1\n              2"
          "cabal-version: 3.0\ndescription:\n  1\n   2\n"

      Hspec.it "indents third line (but not first) past second when hung" $ do
        -- If the first line of the value is on the same line as the name, then
        -- it should never be indented.
        expectGilded
          "cabal-version: 3.0\ndescription: 1\n 2\n  3"
          "cabal-version: 3.0\ndescription:\n  1\n  2\n   3\n"

      Hspec.it "treats tabs the same as spaces" $ do
        -- Yes, each tab only counts as one space.
        expectGilded
          "cabal-version: 3.0\ndescription:\n 1\n\t2"
          "cabal-version: 3.0\ndescription:\n  1\n  2\n"

      Hspec.it "does not insert extra blank lines before comments" $ do
        expectGilded
          "cabal-version: 3.0\ndescription:\n -- c\n 1\n -- d\n 2"
          "cabal-version: 3.0\ndescription:\n  -- c\n  -- d\n  1\n  2\n"

      Hspec.it "does not consider comments for indentation" $ do
        expectGilded
          "cabal-version: 3.0\ndescription:\n  1\n -- c\n    2"
          "cabal-version: 3.0\ndescription:\n  -- c\n  1\n    2\n"

  Hspec.it "properly formats conditionals" $ do
    expectGilded
      "if ! impl ( ghc >= 9.8 )"
      "if !impl(ghc >=9.8)\n"

  Hspec.describe "conditionals" $ do
    Hspec.it "formats variable" $ do
      expectGilded
        "if flag ( x )"
        "if flag(x)\n"

    Hspec.it "formats false" $ do
      expectGilded
        "if false"
        "if false\n"

    Hspec.it "formats upper false" $ do
      expectGilded
        "if False"
        "if false\n"

    Hspec.it "formats true" $ do
      expectGilded
        "if true"
        "if true\n"

    Hspec.it "formats upper true" $ do
      expectGilded
        "if True"
        "if true\n"

    Hspec.it "formats not" $ do
      expectGilded
        "if ! flag ( x )"
        "if !flag(x)\n"

    Hspec.it "formats or" $ do
      expectGilded
        "if flag ( x )||flag ( y )"
        "if flag(x) || flag(y)\n"

    Hspec.it "formats and" $ do
      expectGilded
        "if flag ( x )&&flag ( y )"
        "if flag(x) && flag(y)\n"

    Hspec.it "formats arch" $ do
      expectGilded
        "if arch ( aarch64 )"
        "if arch(aarch64)\n"

    Hspec.it "formats upper arch" $ do
      expectGilded
        "if arch ( AARCH64 )"
        "if arch(aarch64)\n"

    Hspec.it "formats arch alias" $ do
      expectGilded
        "if arch ( arm64 )"
        "if arch(aarch64)\n"

    Hspec.it "formats flag" $ do
      expectGilded
        "if flag ( x )"
        "if flag(x)\n"

    Hspec.it "formats upper flag" $ do
      expectGilded
        "if flag ( X )"
        "if flag(x)\n"

    Hspec.it "formats impl" $ do
      expectGilded
        "if impl ( ghc > 0 )"
        "if impl(ghc >0)\n"

    Hspec.it "formats upper impl" $ do
      expectGilded
        "if impl ( GHC > 0 )"
        "if impl(ghc >0)\n"

    Hspec.it "formats impl without version range" $ do
      expectGilded
        "if impl ( ghc )"
        "if impl(ghc >=0)\n"

    Hspec.it "formats os" $ do
      expectGilded
        "if os ( osx )"
        "if os(osx)\n"

    Hspec.it "formats upper os" $ do
      expectGilded
        "if os ( OSX )"
        "if os(osx)\n"

    Hspec.it "formats os alias" $ do
      expectGilded
        "if os ( darwin )"
        "if os(osx)\n"

    Hspec.it "does not format old elif" $ do
      expectGilded
        "elif flag ( x )"
        "elif flag ( x )\n"

    Hspec.it "formats new elif" $ do
      expectGilded
        "cabal-version: 2.2\nelif flag ( x )"
        "cabal-version: 2.2\nelif flag(x)\n"

    Hspec.it "keeps explicit parentheses" $ do
      expectGilded
        "if ( true )"
        "if (true)\n"

    Hspec.it "formats multiple nots" $ do
      expectGilded
        "if ! ! flag ( a )"
        "if !!flag(a)\n"

    Hspec.it "formats multiple ands" $ do
      expectGilded
        "if flag ( a ) && flag ( b ) && flag ( c )"
        "if flag(a) && flag(b) && flag(c)\n"

    Hspec.it "formats multiple ors" $ do
      expectGilded
        "if flag ( a ) || flag ( b ) || flag ( c )"
        "if flag(a) || flag(b) || flag(c)\n"

    Hspec.it "formats mixed operators" $ do
      expectGilded
        "if ! flag ( a ) && flag ( b ) || flag ( c )"
        "if !flag(a) && flag(b) || flag(c)\n"

  Hspec.describe "license-files" $ do
    -- These tests apply to other "list" fields as well. The license-files
    -- field is just a representative example.

    Hspec.it "removes duplicates" $ do
      expectGilded
        "license-files: f f"
        "license-files: f\n"

    Hspec.it "sorts unquoted" $ do
      expectGilded
        "license-files: g f"
        "license-files:\n  f\n  g\n"

    Hspec.it "sorts mixed" $ do
      expectGilded
        "license-files: \"g\" f"
        "license-files:\n  f\n  g\n"

    Hspec.it "sorts quoted" $ do
      expectGilded
        "license-files: \"g\" \"f\""
        "license-files:\n  f\n  g\n"

    Hspec.it "sorts comma-separated" $ do
      expectGilded
        "license-files: g, f"
        "license-files:\n  f\n  g\n"

    Hspec.it "collects comments at the top" $ do
      -- This isn't ideal, but keeping comments "close" to their original
      -- identifier is very difficult.
      expectGilded
        "license-files:\n -- c\n g\n -- d\n f"
        "license-files:\n  -- c\n  -- d\n  f\n  g\n"

    Hspec.it "leaves invalid fields alone" $ do
      -- This field value is valid in general. Cabal will reject it because the
      -- license-files field has special rules. Perhaps the user is trying to
      -- format something other than a package description. Or maybe they just
      -- made a typo. Either way, we can leave it alone.
      expectGilded
        "license-files: , f ,"
        "license-files: , f ,\n"

  Hspec.it "sorts tested-with" $ do
    expectGilded
      "tested-with: GHC == 9.8.1 , GHC == 9.6.4"
      "tested-with:\n  ghc ==9.6.4\n  ghc ==9.8.1\n"

  Hspec.it "sorts data-files" $ do
    expectGilded
      "data-files: g f"
      "data-files:\n  f\n  g\n"

  Hspec.it "sorts extra-source-files" $ do
    expectGilded
      "extra-source-files: g f"
      "extra-source-files:\n  f\n  g\n"

  Hspec.it "sorts extra-tmp-files" $ do
    expectGilded
      "extra-tmp-files: g f"
      "extra-tmp-files:\n  f\n  g\n"

  Hspec.it "sorts extra-doc-files" $ do
    expectGilded
      "extra-doc-files: g f"
      "extra-doc-files:\n  f\n  g\n"

  Hspec.it "sorts exposed-modules" $ do
    expectGilded
      "library\n exposed-modules: N M"
      "library\n  exposed-modules:\n    M\n    N\n"

  Hspec.it "sorts reexported-modules" $ do
    expectGilded
      "library\n reexported-modules: p:M4 as M5, M2 as M3, q:M6, M1"
      "library\n  reexported-modules:\n    M1,\n    M2 as M3,\n    p:M4 as M5,\n    q:M6\n"

  Hspec.it "sorts signatures" $ do
    expectGilded
      "library\n signatures: N M"
      "library\n  signatures:\n    M\n    N\n"

  Hspec.it "sorts build-tools" $ do
    expectGilded
      "library\n build-tools: q , p == 1"
      "library\n  build-tools:\n    p ==1,\n    q >=0\n"

  Hspec.it "adds a trailing comma to build-tools when possible" $ do
    expectGilded
      "cabal-version: 2.2\nlibrary\n build-tools: p == 1, q == 2"
      "cabal-version: 2.2\n\nlibrary\n  build-tools:\n    p ==1,\n    q ==2,\n"

  Hspec.it "does not add a trailing comma to build-tools with one element" $ do
    expectGilded
      "cabal-version: 2.2\nlibrary\n build-tools: p"
      "cabal-version: 2.2\n\nlibrary\n  build-tools: p >=0\n"

  Hspec.it "sorts build-tool-depends" $ do
    expectGilded
      "library\n build-tool-depends: q:d , p:c == 1"
      "library\n  build-tool-depends:\n    p:c ==1,\n    q:d\n"

  Hspec.it "sorts pkgconfig-depends" $ do
    -- This unusual version range format appears to be a quirk of Cabal's
    -- parser specifically for pkg-config.
    expectGilded
      "library\n pkgconfig-depends: q , p == 1"
      "library\n  pkgconfig-depends:\n    p >=1 && <=1,\n    q\n"

  Hspec.it "sorts frameworks" $ do
    expectGilded
      "library\n frameworks: b a"
      "library\n  frameworks:\n    a\n    b\n"

  Hspec.it "sorts extra-framework-dirs" $ do
    expectGilded
      "library\n extra-framework-dirs: e d"
      "library\n  extra-framework-dirs:\n    d\n    e\n"

  Hspec.it "sorts asm-sources" $ do
    expectGilded
      "library\n asm-sources: g f"
      "library\n  asm-sources:\n    f\n    g\n"

  Hspec.it "sorts cmm-sources" $ do
    expectGilded
      "library\n cmm-sources: g f"
      "library\n  cmm-sources:\n    f\n    g\n"

  Hspec.it "sorts c-sources" $ do
    expectGilded
      "library\n c-sources: g f"
      "library\n  c-sources:\n    f\n    g\n"

  Hspec.it "sorts cxx-sources" $ do
    expectGilded
      "library\n cxx-sources: g f"
      "library\n  cxx-sources:\n    f\n    g\n"

  Hspec.it "sorts js-sources" $ do
    expectGilded
      "library\n js-sources: g f"
      "library\n  js-sources:\n    f\n    g\n"

  Hspec.it "sorts other-modules" $ do
    expectGilded
      "library\n other-modules: N M"
      "library\n  other-modules:\n    M\n    N\n"

  Hspec.it "sorts virtual-modules" $ do
    expectGilded
      "library\n virtual-modules: N M"
      "library\n  virtual-modules:\n    M\n    N\n"

  Hspec.it "sorts autogen-modules" $ do
    expectGilded
      "library\n autogen-modules: N M"
      "library\n  autogen-modules:\n    M\n    N\n"

  Hspec.it "sorts other-languages" $ do
    expectGilded
      "library\n other-languages: b a"
      "library\n  other-languages:\n    a\n    b\n"

  Hspec.it "sorts default-extensions" $ do
    expectGilded
      "library\n default-extensions: b a"
      "library\n  default-extensions:\n    a\n    b\n"

  Hspec.it "sorts known extensions by name" $ do
    expectGilded
      "library\n default-extensions: DerivingVia BlockArguments"
      "library\n  default-extensions:\n    BlockArguments\n    DerivingVia\n"

  Hspec.it "does not sort disabling extensions after enabling ones" $ do
    expectGilded
      "library\n default-extensions: StrictData NoStrictData"
      "library\n  default-extensions:\n    NoStrictData\n    StrictData\n"

  Hspec.it "sorts unknown extensions with known ones" $ do
    expectGilded
      "library\n default-extensions: LambdaCase Imaginary"
      "library\n  default-extensions:\n    Imaginary\n    LambdaCase\n"

  Hspec.it "sorts unknown disabling extensions with known ones" $ do
    expectGilded
      "library\n default-extensions: NoLambdaCase NoImaginary"
      "library\n  default-extensions:\n    NoImaginary\n    NoLambdaCase\n"

  Hspec.it "sorts extensions (issue 29)" $ do
    expectGilded
      "library\n default-extensions: Arrows Imaginary1 NoCPP NoImaginary2 NoUnboxedTuples UnicodeSyntax"
      "library\n  default-extensions:\n    Arrows\n    Imaginary1\n    NoCPP\n    NoImaginary2\n    NoUnboxedTuples\n    UnicodeSyntax\n"

  Hspec.it "sorts other-extensions" $ do
    expectGilded
      "library\n other-extensions: b a"
      "library\n  other-extensions:\n    a\n    b\n"

  Hspec.it "sorts extensions" $ do
    expectGilded
      "library\n extensions: b a"
      "library\n  extensions:\n    a\n    b\n"

  Hspec.it "sorts build-depends" $ do
    expectGilded
      "library\n build-depends: q , p == 1"
      "library\n  build-depends:\n    p ==1,\n    q\n"

  Hspec.it "adds a trailing comma to build-depends when possible" $ do
    expectGilded
      "cabal-version: 2.2\nlibrary\n build-depends: p == 1, q == 2"
      "cabal-version: 2.2\n\nlibrary\n  build-depends:\n    p ==1,\n    q ==2,\n"

  Hspec.it "does not add a trailing comma to build-depends with one element" $ do
    expectGilded
      "cabal-version: 2.2\nlibrary\n build-depends: p"
      "cabal-version: 2.2\n\nlibrary\n  build-depends: p\n"

  Hspec.it "sorts sub-libraries in build-depends" $ do
    expectGilded
      "cabal-version: 3.0\nlibrary\n build-depends: p:{b,a}"
      "cabal-version: 3.0\n\nlibrary\n  build-depends: p:{a, b}\n"

  Hspec.it "removes duplicate options" $ do
    -- This is kind of silly because there's only one possible foreign library
    -- option.
    expectGilded
      "foreign-library x\n options: standalone standalone"
      "foreign-library x\n  options: standalone\n"

  Hspec.it "does not sort hs-source-dirs" $ do
    expectGilded
      "library\n hs-source-dirs: b a"
      "library\n  hs-source-dirs:\n    b\n    a\n"

  Hspec.it "does not sort code-generators" $ do
    expectGilded
      "test-suite x\n code-generators: b, a"
      "test-suite x\n  code-generators:\n    b,\n    a\n"

  Hspec.it "adds a trailing comma to code-generators when possible" $ do
    expectGilded
      "cabal-version: 2.2\ntest-suite x\n code-generators: a, b"
      "cabal-version: 2.2\n\ntest-suite x\n  code-generators:\n    a,\n    b,\n"

  Hspec.it "does not add a trailing comma to code-generators with one element" $ do
    expectGilded
      "cabal-version: 2.2\ntest-suite x\n code-generators: a"
      "cabal-version: 2.2\n\ntest-suite x\n  code-generators: a\n"

  Hspec.it "does not sort cpp-options" $ do
    expectGilded
      "library\n cpp-options: b a"
      "library\n  cpp-options:\n    b\n    a\n"

  Hspec.it "does not sort asm-options" $ do
    expectGilded
      "library\n asm-options: b a"
      "library\n  asm-options:\n    b\n    a\n"

  Hspec.it "does not sort cmm-options" $ do
    expectGilded
      "library\n cmm-options: b a"
      "library\n  cmm-options:\n    b\n    a\n"

  Hspec.it "does not sort cc-options" $ do
    expectGilded
      "library\n cc-options: b a"
      "library\n  cc-options:\n    b\n    a\n"

  Hspec.it "does not sort cxx-options" $ do
    expectGilded
      "library\n cxx-options: b a"
      "library\n  cxx-options:\n    b\n    a\n"

  Hspec.it "does not sort ld-options" $ do
    expectGilded
      "library\n ld-options: b a"
      "library\n  ld-options:\n    b\n    a\n"

  Hspec.it "does not sort hsc2hs-options" $ do
    expectGilded
      "library\n hsc2hs-options: b a"
      "library\n  hsc2hs-options:\n    b\n    a\n"

  Hspec.it "sorts extra-libraries" $ do
    expectGilded
      "library\n extra-libraries: b a"
      "library\n  extra-libraries:\n    a\n    b\n"

  Hspec.it "sorts extra-libraries-static" $ do
    expectGilded
      "library\n extra-libraries-static: b a"
      "library\n  extra-libraries-static:\n    a\n    b\n"

  Hspec.it "sorts extra-ghci-libraries" $ do
    expectGilded
      "library\n extra-ghci-libraries: b a"
      "library\n  extra-ghci-libraries:\n    a\n    b\n"

  Hspec.it "sorts extra-bundled-libraries" $ do
    expectGilded
      "library\n extra-bundled-libraries: b a"
      "library\n  extra-bundled-libraries:\n    a\n    b\n"

  Hspec.it "sorts extra-library-flavours" $ do
    expectGilded
      "library\n extra-library-flavours: b a"
      "library\n  extra-library-flavours:\n    a\n    b\n"

  Hspec.it "sorts extra-dynamic-library-flavours" $ do
    expectGilded
      "library\n extra-dynamic-library-flavours: b a"
      "library\n  extra-dynamic-library-flavours:\n    a\n    b\n"

  Hspec.it "sorts extra-lib-dirs" $ do
    expectGilded
      "library\n extra-lib-dirs: g f"
      "library\n  extra-lib-dirs:\n    f\n    g\n"

  Hspec.it "sorts extra-lib-dirs-static" $ do
    expectGilded
      "library\n extra-lib-dirs-static: g f"
      "library\n  extra-lib-dirs-static:\n    f\n    g\n"

  Hspec.it "sorts include-dirs" $ do
    expectGilded
      "library\n include-dirs: g f"
      "library\n  include-dirs:\n    f\n    g\n"

  Hspec.it "sorts includes" $ do
    expectGilded
      "library\n includes: g f"
      "library\n  includes:\n    f\n    g\n"

  Hspec.it "sorts autogen-includes" $ do
    expectGilded
      "library\n autogen-includes: g f"
      "library\n  autogen-includes:\n    f\n    g\n"

  Hspec.it "sorts install-includes" $ do
    expectGilded
      "library\n install-includes: g f"
      "library\n  install-includes:\n    f\n    g\n"

  Hspec.it "sorts mixins" $ do
    expectGilded
      "library\n mixins: q (M, N as O), p"
      "library\n  mixins:\n    p,\n    q (M, N as O)\n"

  Hspec.it "sorts hiding in mixins" $ do
    expectGilded
      "library\n mixins: p hiding (N, M)"
      "library\n  mixins: p hiding (M, N)\n"

  Hspec.it "sorts modules in mixins" $ do
    expectGilded
      "library\n mixins: p (N, M)"
      "library\n  mixins: p (M, N)\n"

  Hspec.it "sorts hiding in mixins requires" $ do
    expectGilded
      "library\n mixins: p requires hiding (N, M)"
      "library\n  mixins: p requires hiding (M, N)\n"

  Hspec.it "sorts modules in mixins requires" $ do
    expectGilded
      "library\n mixins: p requires (N, M)"
      "library\n  mixins: p requires (M, N)\n"

  Hspec.it "does not sort ghc-options" $ do
    expectGilded
      "library\n ghc-options: b a"
      "library\n  ghc-options:\n    b\n    a\n"

  Hspec.it "does not sort ghcjs-options" $ do
    expectGilded
      "library\n ghcjs-options: b a"
      "library\n  ghcjs-options:\n    b\n    a\n"

  Hspec.it "does not sort ghc-prof-options" $ do
    expectGilded
      "library\n ghc-prof-options: b a"
      "library\n  ghc-prof-options:\n    b\n    a\n"

  Hspec.it "does not sort ghcjs-prof-options" $ do
    expectGilded
      "library\n ghcjs-prof-options: b a"
      "library\n  ghcjs-prof-options:\n    b\n    a\n"

  Hspec.it "does not sort ghc-shared-options" $ do
    expectGilded
      "library\n ghc-shared-options: b a"
      "library\n  ghc-shared-options:\n    b\n    a\n"

  Hspec.it "does not sort ghcjs-shared-options" $ do
    expectGilded
      "library\n ghcjs-shared-options: b a"
      "library\n  ghcjs-shared-options:\n    b\n    a\n"

  Hspec.it "sorts setup-depends" $ do
    expectGilded
      "custom-setup\n setup-depends: b, a"
      "custom-setup\n  setup-depends:\n    a,\n    b\n"

  Hspec.it "sorts sub-libraries in setup-depends" $ do
    expectGilded
      "cabal-version: 3.0\ncustom-setup\n setup-depends: p:{b,a}"
      "cabal-version: 3.0\n\ncustom-setup\n  setup-depends: p:{a, b}\n"

  Hspec.it "discovers an exposed module" $ do
    expectDiscover
      (".", [["M.hs"]])
      "library\n -- cabal-gild: discover\n exposed-modules:"
      "library\n  -- cabal-gild: discover\n  exposed-modules: M\n"

  Hspec.it "discovers an other module" $ do
    expectDiscover
      (".", [["M.hs"]])
      "library\n -- cabal-gild: discover\n other-modules:"
      "library\n  -- cabal-gild: discover\n  other-modules: M\n"

  Hspec.it "discovers a nested module" $ do
    expectDiscover
      (".", [["N", "O.hs"]])
      "library\n -- cabal-gild: discover\n exposed-modules:"
      "library\n  -- cabal-gild: discover\n  exposed-modules: N.O\n"

  Hspec.it "discovers multiple modules" $ do
    expectDiscover
      (".", [["M.hs"], ["N.hs"]])
      "library\n -- cabal-gild: discover\n exposed-modules:"
      "library\n  -- cabal-gild: discover\n  exposed-modules:\n    M\n    N\n"

  Hspec.it "discovers no modules" $ do
    expectGilded
      "library\n -- cabal-gild: discover\n exposed-modules:"
      "library\n  -- cabal-gild: discover\n  exposed-modules:\n"

  Hspec.it "discovers a .lhs file" $ do
    expectDiscover
      (".", [["M.lhs"]])
      "library\n -- cabal-gild: discover\n exposed-modules:"
      "library\n  -- cabal-gild: discover\n  exposed-modules: M\n"

  Hspec.it "discovers a .gc file" $ do
    expectDiscover
      (".", [["M.gc"]])
      "library\n -- cabal-gild: discover\n exposed-modules:"
      "library\n  -- cabal-gild: discover\n  exposed-modules: M\n"

  Hspec.it "discovers a .chs file" $ do
    expectDiscover
      (".", [["M.chs"]])
      "library\n -- cabal-gild: discover\n exposed-modules:"
      "library\n  -- cabal-gild: discover\n  exposed-modules: M\n"

  Hspec.it "discovers a .hsc file" $ do
    expectDiscover
      (".", [["M.hsc"]])
      "library\n -- cabal-gild: discover\n exposed-modules:"
      "library\n  -- cabal-gild: discover\n  exposed-modules: M\n"

  Hspec.it "discovers a .y file" $ do
    expectDiscover
      (".", [["M.y"]])
      "library\n -- cabal-gild: discover\n exposed-modules:"
      "library\n  -- cabal-gild: discover\n  exposed-modules: M\n"

  Hspec.it "discovers a .ly file" $ do
    expectDiscover
      (".", [["M.ly"]])
      "library\n -- cabal-gild: discover\n exposed-modules:"
      "library\n  -- cabal-gild: discover\n  exposed-modules: M\n"

  Hspec.it "discovers a .x file" $ do
    expectDiscover
      (".", [["M.x"]])
      "library\n -- cabal-gild: discover\n exposed-modules:"
      "library\n  -- cabal-gild: discover\n  exposed-modules: M\n"

  Hspec.it "discovers a .cpphs file" $ do
    expectDiscover
      (".", [["M.cpphs"]])
      "library\n -- cabal-gild: discover\n exposed-modules:"
      "library\n  -- cabal-gild: discover\n  exposed-modules: M\n"

  Hspec.it "discovers a .hsig file" $ do
    expectDiscover
      (".", [["M.hsig"]])
      "library\n -- cabal-gild: discover\n exposed-modules:"
      "library\n  -- cabal-gild: discover\n  exposed-modules: M\n"

  Hspec.it "discovers a .lhsig file" $ do
    expectDiscover
      (".", [["M.lhsig"]])
      "library\n -- cabal-gild: discover\n exposed-modules:"
      "library\n  -- cabal-gild: discover\n  exposed-modules: M\n"

  Hspec.it "discovers a signature" $ do
    expectDiscover
      (".", [["S.hsig"]])
      "library\n -- cabal-gild: discover\n signatures:"
      "library\n  -- cabal-gild: discover\n  signatures: S\n"

  Hspec.it "ignores discover pragma separated by comment" $ do
    expectGilded
      "library\n -- cabal-gild: discover\n -- foo\n exposed-modules: M"
      "library\n  -- cabal-gild: discover\n  -- foo\n  exposed-modules: M\n"

  Hspec.it "ignores misplaced discover pragma" $ do
    expectGilded
      "-- cabal-gild: discover\nname: p"
      "-- cabal-gild: discover\nname: p\n"

  Hspec.it "ignores unknown pragma" $ do
    expectGilded
      "-- cabal-gild: unknown"
      "-- cabal-gild: unknown\n"

  Hspec.it "discovers from the currently directory explicitly" $ do
    expectDiscover
      (".", [["M.hs"]])
      "library\n -- cabal-gild: discover .\n exposed-modules:"
      "library\n  -- cabal-gild: discover .\n  exposed-modules: M\n"

  Hspec.it "discovers from multiple directories" $ do
    expectDiscover
      (".", [["d", "M.hs"], ["e", "N.hs"]])
      "library\n -- cabal-gild: discover d e\n exposed-modules:"
      "library\n  -- cabal-gild: discover d e\n  exposed-modules:\n    M\n    N\n"

  Hspec.it "discovers from a quoted directory" $ do
    expectDiscover
      (".", [["d", "M.hs"]])
      "library\n -- cabal-gild: discover \"d\"\n exposed-modules:"
      "library\n  -- cabal-gild: discover \"d\"\n  exposed-modules: M\n"

  Hspec.it "discovers from a directory with a space" $ do
    expectDiscover
      (".", [["s p", "M.hs"]])
      "library\n -- cabal-gild: discover \"s p\"\n exposed-modules:"
      "library\n  -- cabal-gild: discover \"s p\"\n  exposed-modules: M\n"

  Hspec.it "discovers from the current directory by default" $ do
    expectDiscover
      (".", [["M.hs"]])
      "library\n -- cabal-gild: discover\n exposed-modules:"
      "library\n  -- cabal-gild: discover\n  exposed-modules: M\n"

  Hspec.it "allows excluding a path when discovering" $ do
    expectDiscover
      (".", [["M.hs"], ["N.hs"]])
      "library\n -- cabal-gild: discover --exclude M.hs\n exposed-modules:"
      "library\n  -- cabal-gild: discover --exclude M.hs\n  exposed-modules: N\n"

  Hspec.it "allows excluding a nested POSIX path" $ do
    expectDiscover
      (".", [["A", "M.hs"], ["B", "M.hs"]])
      "library\n -- cabal-gild: discover --exclude B/M.hs\n exposed-modules:"
      "library\n  -- cabal-gild: discover --exclude B/M.hs\n  exposed-modules: A.M\n"

  Hspec.it "allows excluding a nested Windows path" $ do
    expectDiscover
      (".", [["A", "M.hs"], ["B", "M.hs"]])
      "library\n -- cabal-gild: discover --exclude B\\M.hs\n exposed-modules:"
      "library\n  -- cabal-gild: discover --exclude B\\M.hs\n  exposed-modules: A.M\n"

  Hspec.it "allows excluding a relative POSIX path" $ do
    expectDiscover
      (".", [["M.hs"], ["N.hs"]])
      "library\n -- cabal-gild: discover --exclude ./M.hs\n exposed-modules:"
      "library\n  -- cabal-gild: discover --exclude ./M.hs\n  exposed-modules: N\n"

  Hspec.it "allows excluding a relative Windows path" $ do
    expectDiscover
      (".", [["M.hs"], ["N.hs"]])
      "library\n -- cabal-gild: discover --exclude .\\M.hs\n exposed-modules:"
      "library\n  -- cabal-gild: discover --exclude .\\M.hs\n  exposed-modules: N\n"

  Hspec.it "allows excluding multiple paths" $ do
    expectDiscover
      (".", [["M.hs"], ["N.hs"], ["O.hs"]])
      "library\n -- cabal-gild: discover --exclude M.hs --exclude O.hs\n exposed-modules:"
      "library\n  -- cabal-gild: discover --exclude M.hs --exclude O.hs\n  exposed-modules: N\n"

  Hspec.it "allows excluding paths that don't match anything" $ do
    expectDiscover
      (".", [["M.hs"]])
      "library\n -- cabal-gild: discover --exclude N.hs\n exposed-modules:"
      "library\n  -- cabal-gild: discover --exclude N.hs\n  exposed-modules: M\n"

  Hspec.it "treats excluded paths relative to cabal file" $ do
    let d = "input"
        (a, s, w) =
          runGild
            ["--input", FilePath.combine d "io.cabal"]
            [(Input.File $ FilePath.combine d "io.cabal", String.toUtf8 "library\n -- cabal-gild: discover src --exclude src/N.hs\n exposed-modules:")]
            (d, [["src", "M.hs"], ["src", "N.hs"]])
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.singleton Output.Stdout (String.toUtf8 "library\n  -- cabal-gild: discover src --exclude src/N.hs\n  exposed-modules: M\n")

  Hspec.it "allows excluding simple wildcards" $ do
    expectDiscover
      (".", [["M.hs"], ["MSpec.hs"]])
      "library\n -- cabal-gild: discover --exclude *Spec.hs\n exposed-modules:"
      "library\n  -- cabal-gild: discover --exclude *Spec.hs\n  exposed-modules: M\n"

  Hspec.it "allows excluding complex wildcards" $ do
    expectDiscover
      (".", [["A.hs"], ["A", "B.hs"], ["X", "C.hs"], ["A", "X", "D.hs"]])
      "library\n -- cabal-gild: discover --exclude **/X/**/*.hs\n exposed-modules:"
      "library\n  -- cabal-gild: discover --exclude **/X/**/*.hs\n  exposed-modules:\n    A\n    A.B\n"

  Hspec.it "fails when discovering with an unknown option" $ do
    let (a, s, w) =
          runGild
            []
            [(Input.Stdin, String.toUtf8 "-- cabal-gild: discover --unknown\nsignatures:")]
            (".", [])
    a `shouldBeFailure` UnknownOption.UnknownOption "--unknown"
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.empty

  Hspec.it "fails when discovering with an invalid option" $ do
    let (a, s, w) =
          runGild
            []
            [(Input.Stdin, String.toUtf8 "-- cabal-gild: discover --exclude\nsignatures:")]
            (".", [])
    a `shouldBeFailure` InvalidOption.InvalidOption "option `--exclude' requires an argument PATTERN"
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.empty

  Hspec.it "retains comments when discovering" $ do
    expectDiscover
      (".", [["M.hs"]])
      "library\n -- cabal-gild: discover\n exposed-modules:\n  -- c\n  N"
      "library\n  -- cabal-gild: discover\n  exposed-modules:\n    -- c\n    M\n"

  Hspec.it "concatenates comments when discovering" $ do
    expectDiscover
      (".", [["M.hs"]])
      "library\n -- cabal-gild: discover\n exposed-modules:\n  -- c\n  N\n  -- d\n  O"
      "library\n  -- cabal-gild: discover\n  exposed-modules:\n    -- c\n    -- d\n    M\n"

  Hspec.it "retains comments even when no modules are discovered" $ do
    expectGilded
      "library\n -- cabal-gild: discover\n exposed-modules:\n  -- c\n  N"
      "library\n  -- c\n  -- cabal-gild: discover\n  exposed-modules:\n"

  Hspec.it "parses an empty brace section" $ do
    expectGilded
      "s{}"
      "s\n"

  Hspec.it "parses a brace section with a layout field" $ do
    expectGilded
      "s{f:x}"
      "s\n  f: x\n"

  Hspec.it "parses a brace section with a brace field" $ do
    expectGilded
      "s{f:{x}}"
      "s\n  f: x\n"

  Hspec.it "strips blanks from a layout field in a brace section" $ do
    expectGilded
      "s { f : x } "
      "s\n  f: x\n"

  Hspec.it "strips blanks from a brace field in a brace section" $ do
    expectGilded
      "s { f : { x } } "
      "s\n  f: x\n"

  Hspec.it "parses a brace section with multiple fields" $ do
    expectGilded
      "s { f : { x } g : { y } } "
      "s\n  f: x\n  g: y\n"

  Hspec.it "parses a nested brace section" $ do
    expectGilded
      "s{t{}}"
      "s\n  t\n"

  Hspec.it "groups 'else' with 'if'" $ do
    expectGilded
      "if p\n a\nelse\n b"
      "if p\n  a\nelse\n  b\n"

  Hspec.it "does not group old 'elif' with 'if'" $ do
    expectGilded
      "if p\n a\nelif q\n b"
      "if p\n  a\n\nelif q\n  b\n"

  Hspec.it "groups new 'elif' with 'if'" $ do
    expectGilded
      "cabal-version: 2.2\nif p\n a\nelif q\n b"
      "cabal-version: 2.2\n\nif p\n  a\nelif q\n  b\n"

  Hspec.it "groups 'else' with 'elif'" $ do
    expectGilded
      "cabal-version: 2.2\nif p\n a\nelif q\n b\nelse\n c"
      "cabal-version: 2.2\n\nif p\n  a\nelif q\n  b\nelse\n  c\n"

  Hspec.it "does not group 'else' with anything else" $ do
    expectGilded
      "library\nelse p\n a"
      "library\n\nelse p\n  a\n"

  Hspec.it "does not group 'elif' with anything else" $ do
    expectGilded
      "library\nelif p\n a"
      "library\n\nelif p\n  a\n"

  Hspec.it "keeps output on multiple lines" $ do
    expectGilded
      "f:\n a"
      "f:\n  a\n"

  Hspec.it "keeps output on multiple lines for formatted fields" $ do
    expectGilded
      "library\n build-depends:\n  base<5"
      "library\n  build-depends:\n    base <5\n"

  Hspec.it "keeps output on multiple lines for pragmas" $ do
    expectDiscover
      (".", [["M.hs"]])
      "library\n -- cabal-gild: discover\n exposed-modules:\n  ..."
      "library\n  -- cabal-gild: discover\n  exposed-modules:\n    M\n"

  Hspec.it "does not put a blank line after an empty field" $ do
    expectGilded
      "f:\ng: a"
      "f:\ng: a\n"

  Hspec.it "supports including a module" $ do
    expectDiscover
      (".", [["M.hs"], ["N.hs"]])
      "library\n -- cabal-gild: discover --include M.hs\n exposed-modules:"
      "library\n  -- cabal-gild: discover --include M.hs\n  exposed-modules: M\n"

  Hspec.it "supports including a pattern" $ do
    expectDiscover
      (".", [["M1.hs"], ["M2.hs"], ["N.hs"]])
      "library\n -- cabal-gild: discover --include M*.hs\n exposed-modules:"
      "library\n  -- cabal-gild: discover --include M*.hs\n  exposed-modules:\n    M1\n    M2\n"

  Hspec.it "supports including from multiple directories" $ do
    -- Note that the include pattern needs to be `**/*X.hs` rather than just
    -- `*X.hs`. That's because it's relative to the package description, not
    -- the directories listed in the discover pragma.
    expectDiscover
      (".", [["a", "AX.hs"], ["a", "M.hs"], ["b", "BX.hs"], ["b", "N.hs"]])
      "library\n -- cabal-gild: discover a b --include **/*X.hs\n exposed-modules:"
      "library\n  -- cabal-gild: discover a b --include **/*X.hs\n  exposed-modules:\n    AX\n    BX\n"

  Hspec.it "supports including with multiple patterns" $ do
    expectDiscover
      (".", [["M.hs"], ["N.hs"], ["O.hs"]])
      "library\n -- cabal-gild: discover --include M.hs --include N.hs\n exposed-modules:"
      "library\n  -- cabal-gild: discover --include M.hs --include N.hs\n  exposed-modules:\n    M\n    N\n"

  Hspec.it "supports including and excluding at the same time" $ do
    expectDiscover
      (".", [["XA.hs"], ["XASpec.hs"]])
      "library\n -- cabal-gild: discover --include X*.hs --exclude *Spec.hs\n exposed-modules:"
      "library\n  -- cabal-gild: discover --include X*.hs --exclude *Spec.hs\n  exposed-modules: XA\n"

  Hspec.it "does not discover invalid module names" $ do
    -- `a.M` is not a valid module name because `a` is lowercase. So it should
    -- not be discovered even though it is included.
    expectDiscover
      (".", [["a", "M.hs"]])
      "library\n -- cabal-gild: discover --include a/**\n exposed-modules:"
      "library\n  -- cabal-gild: discover --include a/**\n  exposed-modules:\n"

  Hspec.it "discovers valid module names" $ do
    -- Unlike the previous test, the module `M` should be discovered because
    -- the directory `a` will be stripped off.
    expectDiscover
      (".", [["a", "M.hs"]])
      "library\n -- cabal-gild: discover a --include a/**\n exposed-modules:"
      "library\n  -- cabal-gild: discover a --include a/**\n  exposed-modules: M\n"

  Hspec.it "treats included patterns relative to cabal file" $ do
    let d = "input"
        (a, s, w) =
          runGild
            ["--input", FilePath.combine d "io.cabal"]
            [(Input.File $ FilePath.combine d "io.cabal", String.toUtf8 "library\n -- cabal-gild: discover src --include src/M.hs\n exposed-modules:")]
            (d, [["src", "M.hs"], ["src", "N.hs"]])
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.singleton Output.Stdout (String.toUtf8 "library\n  -- cabal-gild: discover src --include src/M.hs\n  exposed-modules: M\n")

  Hspec.it "discovers asm-sources" $ do
    expectDiscover
      (".", [["example.txt"]])
      "-- cabal-gild: discover\nasm-sources:"
      "-- cabal-gild: discover\nasm-sources: example.txt\n"

  Hspec.it "discovers c-sources" $ do
    expectDiscover
      (".", [["example.txt"]])
      "-- cabal-gild: discover\nc-sources:"
      "-- cabal-gild: discover\nc-sources: example.txt\n"

  Hspec.it "discovers cxx-sources" $ do
    expectDiscover
      (".", [["example.txt"]])
      "-- cabal-gild: discover\ncxx-sources:"
      "-- cabal-gild: discover\ncxx-sources: example.txt\n"

  Hspec.it "discovers data-files" $ do
    expectDiscover
      (".", [["example.txt"]])
      "-- cabal-gild: discover\ndata-files:"
      "-- cabal-gild: discover\ndata-files: example.txt\n"

  Hspec.it "discovers extra-doc-files" $ do
    expectDiscover
      (".", [["example.txt"]])
      "-- cabal-gild: discover\nextra-doc-files:"
      "-- cabal-gild: discover\nextra-doc-files: example.txt\n"

  Hspec.it "discovers extra-source-files" $ do
    expectDiscover
      (".", [["example.txt"]])
      "-- cabal-gild: discover\nextra-source-files:"
      "-- cabal-gild: discover\nextra-source-files: example.txt\n"

  Hspec.it "discovers includes" $ do
    expectDiscover
      (".", [["example.txt"]])
      "-- cabal-gild: discover\nincludes:"
      "-- cabal-gild: discover\nincludes: example.txt\n"

  Hspec.it "discovers install-includes" $ do
    expectDiscover
      (".", [["example.txt"]])
      "-- cabal-gild: discover\ninstall-includes:"
      "-- cabal-gild: discover\ninstall-includes: example.txt\n"

  Hspec.it "discovers js-sources" $ do
    expectDiscover
      (".", [["example.txt"]])
      "-- cabal-gild: discover\njs-sources:"
      "-- cabal-gild: discover\njs-sources: example.txt\n"

  Hspec.it "discovers license-files" $ do
    expectDiscover
      (".", [["example.txt"]])
      "-- cabal-gild: discover\nlicense-files:"
      "-- cabal-gild: discover\nlicense-files: example.txt\n"

  Hspec.it "floats comments on unknown fields" $ do
    expectGilded
      "unknown-field:\n the\n -- some comment\n value"
      "unknown-field:\n  -- some comment\n  the\n  value\n"

  Hspec.it "floats comments when parsing field fails" $ do
    expectGilded
      "build-depends:\n >> no\n -- comment\n parse"
      "build-depends:\n  -- comment\n  >> no\n  parse\n"

  Hspec.it "only discovers modules in given directories" $ do
    expectDiscover
      (".", [["Setup.hs"], ["source", "Example.hs"]])
      "-- cabal-gild: discover source\nexposed-modules:"
      "-- cabal-gild: discover source\nexposed-modules: Example\n"

  Hspec.around_ withTemporaryDirectory
    . Hspec.it "discovers modules on the file system"
    $ do
      -- Although we already have pure tests for this behavior, it's important
      -- to ensure that both POSIX and Windows paths are supported on the real
      -- file system.
      writeFile "i.cabal" $
        unlines
          [ "library",
            "  -- cabal-gild: discover --exclude=.\\M2.hs --exclude=N/M1.hs",
            "  exposed-modules:"
          ]
      writeFile "M1.hs" ""
      writeFile "M2.hs" ""
      Directory.createDirectory "N"
      writeFile (FilePath.combine "N" "M1.hs") ""
      writeFile (FilePath.combine "N" "M2.hs") ""
      Gild.mainWith ["--input=i.cabal", "--output=r.cabal"]
      readFile "r.cabal"
        `Hspec.shouldReturn` unlines
          [ "library",
            "  -- cabal-gild: discover --exclude=.\\M2.hs --exclude=N/M1.hs",
            "  exposed-modules:",
            "    M1",
            "    N.M2"
          ]
      d <- Directory.getCurrentDirectory
      Gild.mainWith ["--input", FilePath.combine d "i.cabal", "--output=a.cabal"]
      readFile "a.cabal"
        `Hspec.shouldReturn` unlines
          [ "library",
            "  -- cabal-gild: discover --exclude=.\\M2.hs --exclude=N/M1.hs",
            "  exposed-modules:",
            "    M1",
            "    N.M2"
          ]

  Hspec.sequential $ Hspec.describe "find cabal file if stdin is a terminal" $ do
    Hspec.around_ withTemporaryDirectory
      . Hspec.it "successfully determine that stdin is terminal and find/format the cabal file"
      $ do
        let filePath = "stdin-is-terminal.cabal"
            fileData =
              unlines
                [ "cabal-version: 2.2",
                  "library",
                  "  autogen-modules: Paths_cabal_gild"
                ]
        writeFile filePath fileData
        readFile filePath `Hspec.shouldReturn` fileData
        Gild.mainWith []
        formattedData <- readFile filePath
        formattedData `Hspec.shouldNotBe` fileData
        let f = filter (not . Char.isSpace)
        f formattedData `Hspec.shouldBe` f fileData

    Hspec.it "stdin is not terminal and should be used instead of finding the cabal file" $ do
      let fileData = ByteString.pack "cabal-version: 2.2\n"
      TestMain.ProcessResult {TestMain.prStdout = out, TestMain.prExitCode = code} <-
        TestMain.captureProcessResult $
          TestMain.withStdin fileData $
            Gild.mainWith []
      out `Hspec.shouldBe` fileData
      code `Hspec.shouldBe` TestMain.ExitSuccess

    Hspec.around_ withTemporaryDirectory
      . Hspec.it "fails if no cabal file is found"
      $ do
        TestMain.ProcessResult {TestMain.prExitCode = code} <-
          TestMain.captureProcessResult $
            Gild.mainWith []
        code `Hspec.shouldNotBe` TestMain.ExitSuccess

    Hspec.around_ withTemporaryDirectory
      . Hspec.it "fails if more than one cabal file is found"
      $ do
        writeFile "0.cabal" "0"
        writeFile "1.cabal" "1"
        TestMain.ProcessResult {TestMain.prExitCode = code} <-
          TestMain.captureProcessResult $
            Gild.mainWith []
        code `Hspec.shouldNotBe` TestMain.ExitSuccess

withTemporaryDirectory :: IO () -> IO ()
withTemporaryDirectory =
  Temp.withSystemTempDirectory "cabal-gild"
    . flip Directory.withCurrentDirectory

shouldBeFailure ::
  (Stack.HasCallStack, Eq e, Exception.Exception e, Show a) =>
  Either Exception.SomeException a ->
  e ->
  Hspec.Expectation
shouldBeFailure result expected = case result of
  Left exception -> case Exception.fromException exception of
    Just actual -> actual `Hspec.shouldBe` expected
    x -> x `Hspec.shouldSatisfy` Maybe.isJust
  x -> x `Hspec.shouldSatisfy` Either.isLeft

expectGilded :: (Stack.HasCallStack) => String -> String -> Hspec.Expectation
expectGilded = expectDiscover (".", [])

expectStable ::
  (Stack.HasCallStack) =>
  (String, [[String]]) ->
  ByteString.ByteString ->
  Hspec.Expectation
expectStable files input = do
  let (a, s, w) = runGild [] [(Input.Stdin, input)] files
  a `Hspec.shouldSatisfy` Either.isRight
  w `Hspec.shouldBe` []
  output <- case Map.toList s of
    [(Output.Stdout, x)] -> pure x
    _ -> fail $ "impossible: " <> show s
  output `Hspec.shouldBe` input

expectDiscover ::
  (Stack.HasCallStack) =>
  (String, [[String]]) ->
  String ->
  String ->
  Hspec.Expectation
expectDiscover files input expected = do
  let (a, s, w) = runGild [] [(Input.Stdin, String.toUtf8 input)] files
  a `Hspec.shouldSatisfy` Either.isRight
  w `Hspec.shouldBe` []
  actual <- case Map.toList s of
    [(Output.Stdout, x)] -> pure x
    _ -> fail $ "impossible: " <> show s
  actual `Hspec.shouldBe` String.toUtf8 expected
  expectStable files actual

runGild ::
  [String] ->
  [(Input.Input, ByteString.ByteString)] ->
  (String, [[String]]) ->
  (Either E (), S, W)
runGild arguments inputs files =
  runTest
    (Gild.mainWith arguments)
    ( Map.fromList inputs,
      fmap FilePath.joinPath <$> uncurry Map.singleton files
    )
    Map.empty

expectException ::
  (Stack.HasCallStack, Eq e, Exception.Exception e) =>
  [String] ->
  e ->
  Hspec.Expectation
expectException flags exception = do
  let (a, s, w) = runGild flags [] (".", [])
  a `shouldBeFailure` exception
  w `Hspec.shouldBe` []
  s `Hspec.shouldBe` Map.empty

type Test = TestT Identity.Identity

runTest :: Test a -> R -> S -> (Either E a, S, W)
runTest t r = Identity.runIdentity . RWST.runRWST (ExceptT.runExceptT $ runTestT t) r

type E = Exception.SomeException

type R = (Map.Map Input.Input ByteString.ByteString, Map.Map FilePath [FilePath])

type S = Map.Map Output.Output ByteString.ByteString

type W = [String]

newtype TestT m a = TestT
  { runTestT :: ExceptT.ExceptT E (RWST.RWST R W S m) a
  }
  deriving (Applicative, Functor, Monad)

instance (Monad m) => MonadLog.MonadLog (TestT m) where
  logLn = TestT . Trans.lift . RWST.tell . pure

instance (Monad m) => MonadRead.MonadRead (TestT m) where
  read k = do
    m <- TestT . Trans.lift . RWST.asks $ Map.lookup k . fst
    case m of
      Nothing -> Exception.throwM . userError $ "read " <> show k
      Just x -> pure x

instance (Monad m) => Exception.MonadThrow (TestT m) where
  throwM = TestT . ExceptT.throwE . Exception.toException

instance (Monad m) => MonadWalk.MonadWalk (TestT m) where
  walk d i x = do
    result <- TestT . Trans.lift . RWST.asks $ Map.lookup d . snd
    case result of
      Nothing -> Exception.throwM . userError $ "walk " <> show d
      Just fs ->
        pure $
          filter
            (\f -> any (FilePattern.?== f) i && not (any (FilePattern.?== f) x))
            fs

instance (Monad m) => MonadWrite.MonadWrite (TestT m) where
  write k = TestT . Trans.lift . RWST.modify . Map.insert k

instance (Monad m) => MonadHandle.MonadHandle (TestT m) where
  stdinIsTerminalDevice = pure False
