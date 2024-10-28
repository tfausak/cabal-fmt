-- | This module defines the main entry point for the application.
module CabalGild.Unstable.Main where

import qualified CabalGild.Unstable.Action.AttachComments as AttachComments
import qualified CabalGild.Unstable.Action.EvaluatePragmas as EvaluatePragmas
import qualified CabalGild.Unstable.Action.ExtractComments as ExtractComments
import qualified CabalGild.Unstable.Action.FormatFields as FormatFields
import qualified CabalGild.Unstable.Action.GetCabalVersion as GetCabalVersion
import qualified CabalGild.Unstable.Action.ReflowText as ReflowText
import qualified CabalGild.Unstable.Action.Render as Render
import qualified CabalGild.Unstable.Action.StripBlanks as StripBlanks
import qualified CabalGild.Unstable.Class.MonadHandle as MonadHandle
import qualified CabalGild.Unstable.Class.MonadLog as MonadLog
import qualified CabalGild.Unstable.Class.MonadRead as MonadRead
import qualified CabalGild.Unstable.Class.MonadWalk as MonadWalk
import qualified CabalGild.Unstable.Class.MonadWrite as MonadWrite
import qualified CabalGild.Unstable.Exception.CheckFailure as CheckFailure
import qualified CabalGild.Unstable.Exception.ParseError as ParseError
import qualified CabalGild.Unstable.Extra.ByteString as ByteString
import qualified CabalGild.Unstable.Type.Config as Config
import qualified CabalGild.Unstable.Type.Context as Context
import qualified CabalGild.Unstable.Type.Flag as Flag
import qualified CabalGild.Unstable.Type.Input as Input
import qualified CabalGild.Unstable.Type.Leniency as Leniency
import qualified CabalGild.Unstable.Type.Mode as Mode
import qualified CabalGild.Unstable.Type.Output as Output
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Distribution.Fields as Fields
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

-- | This is the main entry point for the application. It gets the command line
-- arguments and then hands things off to 'mainWith'. If any exceptions are
-- thrown, they will be handled by 'onException'.
defaultMain :: IO ()
defaultMain = Exception.handle onException $ do
  arguments <- Environment.getArgs
  mainWith arguments

-- | If the exception was an 'Exit.ExitCode', simply exit with that code.
-- Otherwise handle exceptions by printing them to STDERR using
-- 'Exception.displayException' instead of 'show'. Then exit with a failing
-- status code.
onException :: Exception.SomeException -> IO a
onException e = case Exception.fromException e of
  Just exitCode -> Exit.exitWith exitCode
  Nothing -> do
    IO.hPutStrLn IO.stderr $ Exception.displayException e
    Exit.exitFailure

-- | The actual logic for the command line application. This is written using
-- constraints so that it can be run in pure code if so desired. But most often
-- this will be run in 'IO'.
mainWith ::
  ( MonadLog.MonadLog m,
    MonadRead.MonadRead m,
    Exception.MonadThrow m,
    MonadWalk.MonadWalk m,
    MonadWrite.MonadWrite m,
    MonadHandle.MonadHandle m
  ) =>
  [String] ->
  m ()
mainWith arguments = do
  flags <- Flag.fromArguments arguments
  config <- Config.fromFlags flags
  context <- Context.fromConfig config

  input <- MonadRead.read $ Context.input context
  output <- format (Context.stdin context) input

  let formatted = check (Context.crlf context) input output
  case Context.mode context of
    Mode.Check -> Monad.unless formatted $ Exception.throwM CheckFailure.CheckFailure
    Mode.Format -> case (Context.input context, Context.output context) of
      (Input.File i, Output.File o) | formatted, i == o -> pure ()
      (_, o) -> MonadWrite.write o output

-- | Formats the given input using the provided file path as the apparent
-- source file (see 'Context.stdin'). An exception will be thrown if the input
-- is invalid. The 'MonadWalk.MonadWalk' constraint is used to discover modules
-- on the file system. Typically @m@ will be 'IO'.
format ::
  (Exception.MonadThrow m, MonadWalk.MonadWalk m) =>
  FilePath ->
  ByteString.ByteString ->
  m ByteString.ByteString
format filePath input = do
  fields <-
    either (Exception.throwM . ParseError.ParseError) pure $
      Fields.readFields input
  let csv = GetCabalVersion.fromFields fields
      comments = ExtractComments.fromByteString input
  ( StripBlanks.run
      Monad.>=> AttachComments.run
      Monad.>=> ReflowText.run csv
      Monad.>=> EvaluatePragmas.run filePath
      Monad.>=> FormatFields.run csv
      Monad.>=> Render.run csv
    )
    (fields, comments)

-- | Returns true if the output is formatted correctly, false otherwise.
check ::
  Leniency.Leniency ->
  ByteString.ByteString ->
  ByteString.ByteString ->
  Bool
check leniency input output = case leniency of
  Leniency.Lenient ->
    let lf = ByteString.singleton 0x0a
        crlf = ByteString.cons 0x0d lf
     in output == ByteString.replace crlf lf input
  Leniency.Strict -> output == input
