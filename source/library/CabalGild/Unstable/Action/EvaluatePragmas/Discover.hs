module CabalGild.Unstable.Action.EvaluatePragmas.Discover where

import qualified CabalGild.Unstable.Class.MonadWalk as MonadWalk
import qualified CabalGild.Unstable.Exception.InvalidOption as InvalidOption
import qualified CabalGild.Unstable.Exception.UnknownOption as UnknownOption
import qualified CabalGild.Unstable.Extra.FieldLine as FieldLine
import qualified CabalGild.Unstable.Extra.FilePath as FilePath
import qualified CabalGild.Unstable.Extra.ModuleName as ModuleName
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Extra.String as String
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.DiscoverTarget as DiscoverTarget
import qualified CabalGild.Unstable.Type.Pragma as Pragma
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Data.Containers.ListUtils as List
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.Compat.Lens as Lens
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Fields as Fields
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Utils.Generic as Utils
import qualified System.Console.GetOpt as GetOpt
import qualified System.FilePath as FilePath

-- | Evaluates pragmas within the given field. Or, if the field is a section,
-- evaluates pragmas recursively within the fields of the section.
field ::
  (Exception.MonadThrow m, MonadWalk.MonadWalk m, Show p, Show q) =>
  FilePath ->
  Fields.Field (p, [Comment.Comment q]) ->
  m (Fields.Field (p, [Comment.Comment q]))
field p f = case f of
  Fields.Field n fls -> fmap (Maybe.fromMaybe f) . MaybeT.runMaybeT $ do
    dt <-
      maybe Applicative.empty pure $
        Map.lookup (Name.value n) relevantFieldNames
    comment <- MaybeT.hoistMaybe . Utils.safeLast . snd $ Name.annotation n
    Pragma.Pragma (Discover ds) <- MaybeT.hoistMaybe . Parsec.simpleParsecBS $ Comment.value comment
    discover p n fls dt ds
  Fields.Section n sas fs -> Fields.Section n sas <$> traverse (field p) fs

newtype Discover = Discover [String]

instance Parsec.Parsec Discover where
  parsec = do
    Monad.void $ CharParsing.string "discover"
    arguments <-
      Monad.mplus
        (CharParsing.skipSpaces1 *> CharParsing.sepBy Parsec.parsec CharParsing.skipSpaces1)
        ([] <$ CharParsing.spaces)
    CharParsing.eof
    pure . Discover $ fmap Newtypes.getToken' arguments

-- | If modules are discovered for a field, that fields lines are completely
-- replaced.
discover ::
  (Exception.MonadThrow m, MonadWalk.MonadWalk m) =>
  FilePath ->
  Fields.Name (p, [c]) ->
  [Fields.FieldLine (p, [c])] ->
  DiscoverTarget.DiscoverTarget ->
  [String] ->
  MaybeT.MaybeT m (Fields.Field (p, [c]))
discover p n fls dt ds = do
  let (flgs, args, opts, errs) =
        GetOpt.getOpt'
          GetOpt.Permute
          [ GetOpt.Option [] ["include"] (GetOpt.ReqArg Right "PATTERN") "",
            GetOpt.Option [] ["exclude"] (GetOpt.ReqArg Left "PATTERN") ""
          ]
          ds
  let (excs, incs) = Either.partitionEithers flgs
  mapM_ (Exception.throwM . UnknownOption.fromString) opts
  mapM_ (Exception.throwM . InvalidOption.fromString) errs
  let root = FilePath.dropTrailingPathSeparator . clean $ FilePath.takeDirectory p
      directories =
        List.nubOrd
          . fmap clean
          $ if null args then ["."] else args
      exclusions = List.nubOrd $ fmap clean excs
      inclusions =
        List.nubOrd
          . fmap clean
          $ if null incs then fmap (`FilePath.combine` "**") directories else incs
  files <- Trans.lift $ MonadWalk.walk root inclusions exclusions
  let comments = concatMap (snd . FieldLine.annotation) fls
      position =
        maybe (fst $ Name.annotation n) (fst . FieldLine.annotation) $
          Maybe.listToMaybe fls
      fieldLines = case dt of
        DiscoverTarget.Modules ->
          zipWith ModuleName.toFieldLine ((,) position <$> comments : repeat [])
            . Maybe.mapMaybe (toModuleName directories)
            $ Maybe.mapMaybe (stripAnyExtension extensions . clean) files
        DiscoverTarget.Files ->
          zipWith
            (\a -> Fields.FieldLine a . String.toUtf8)
            ((,) position <$> comments : repeat [])
            files
      -- This isn't great, but the comments have to go /somewhere/.
      name =
        if null fieldLines
          then Lens.over (Name.annotationLens . Lens._2) (comments <>) n
          else n
  pure $ Fields.Field name fieldLines

-- | Converts separators into POSIX format and then normalizes the result.
clean :: FilePath -> FilePath
clean = FilePath.normalise . FilePath.toPosixSeparators

-- | These are the names of the fields that can have this action applied to
-- them.
relevantFieldNames :: Map.Map Fields.FieldName DiscoverTarget.DiscoverTarget
relevantFieldNames =
  Map.mapKeys String.toUtf8 . Map.fromList $
    [ ("asm-sources", DiscoverTarget.Files),
      ("c-sources", DiscoverTarget.Files),
      ("cxx-sources", DiscoverTarget.Files),
      ("data-files", DiscoverTarget.Files),
      ("exposed-modules", DiscoverTarget.Modules),
      ("extra-doc-files", DiscoverTarget.Files),
      ("extra-source-files", DiscoverTarget.Files),
      ("includes", DiscoverTarget.Files),
      ("install-includes", DiscoverTarget.Files),
      ("js-sources", DiscoverTarget.Files),
      ("license-files", DiscoverTarget.Files),
      ("other-modules", DiscoverTarget.Modules),
      ("signatures", DiscoverTarget.Modules)
    ]

-- | Attempts to strip any of the given extensions from the file path. If any
-- of them succeed, the result is returned. Otherwise 'Nothing' is returned.
stripAnyExtension :: Set.Set String -> FilePath -> Maybe String
stripAnyExtension es p =
  Maybe.listToMaybe
    . Maybe.mapMaybe (`FilePath.stripExtension` p)
    $ Set.toList es

-- | The set of extensions that should be discovered by this pragma. Any file
-- with one of these extensions will be discovered.
--
-- <https://cabal.readthedocs.io/en/3.10/cabal-package.html#modules-and-preprocessors>
extensions :: Set.Set String
extensions =
  Set.fromList
    [ "chs",
      "cpphs",
      "gc",
      "hs",
      "hsc",
      "hsig",
      "lhs",
      "lhsig",
      "ly",
      "x",
      "y"
    ]

-- | Attempts to convert a file path (without an extension) into a module name
-- by making it relative to one of the given directories.
toModuleName :: [FilePath] -> FilePath -> Maybe ModuleName.ModuleName
toModuleName ds f =
  Maybe.listToMaybe $
    Maybe.mapMaybe (ModuleName.fromFilePath . flip FilePath.makeRelative f) ds
