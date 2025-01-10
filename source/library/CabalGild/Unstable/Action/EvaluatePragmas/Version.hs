{-# LANGUAGE CPP #-}

module CabalGild.Unstable.Action.EvaluatePragmas.Version where

import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Pragma as Pragma
import qualified Data.String as String
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec

field ::
  () =>
  Fields.Field (p, [Comment.Comment q]) ->
  Fields.Field (p, [Comment.Comment q])
field = fmap $ fmap $ fmap version

data Version = Version
  deriving (Eq, Show)

instance Parsec.Parsec Version where
  parsec = Version <$ CharParsing.string cabalVersionPragma

cabalVersionPragma :: (String.IsString a) => a
cabalVersionPragma = String.fromString "version"

version ::
  () =>
  Comment.Comment q ->
  Comment.Comment q
version comment = case Parsec.simpleParsecBS $ Comment.value comment of
  Nothing -> comment
  Just (Pragma.Pragma Version) -> comment {Comment.value = String.fromString versionComment}
  where
    versionComment = " " <> Pragma.cabalGildPragmaPrefix <> " " <> cabalVersionPragma <> " " <> CURRENT_PACKAGE_VERSION
