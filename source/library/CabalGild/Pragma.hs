{-# LANGUAGE OverloadedStrings #-}

module CabalGild.Pragma where

import CabalGild.Comments
import CabalGild.Glob
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as BS
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.ModuleName as C
import qualified Distribution.Parsec as C
import qualified Distribution.Parsec.FieldLineStream as C

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Pragma
  = FieldPragma FieldPragma
  | GlobalPragma GlobalPragma
  deriving (Show)

-- | Pragmas applied per field
data FieldPragma
  = PragmaExpandModules FilePath [C.ModuleName]
  | PragmaGlobFiles Glob
  | PragmaFragment FilePath
  deriving (Show)

-- | Pragmas affecting global output
data GlobalPragma
  = PragmaOptIndent Int
  | PragmaOptTabular Bool
  deriving (Show)

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- | Parse pragma from 'ByteString'.
--
-- An error ('Left') is reported only if input 'ByteString' starts with @-- cabal-gild:@.
parsePragma :: BS.ByteString -> Either String (Maybe Pragma)
parsePragma bs = case dropPrefix bs of
  Nothing -> Right Nothing
  Just bs' -> Bifunctor.bimap show Just $ C.runParsecParser parser "<input>" $ C.fieldLineStreamFromBS bs'
  where
    dropPrefix bs0 = do
      bs1 <- BS.stripPrefix "--" bs0
      bs2 <- BS.stripPrefix "cabal-gild:" (stripWhitespace bs1)
      return (stripWhitespace bs2)

    parser :: C.ParsecParser Pragma
    parser = do
      t <- C.parsecToken
      case t of
        "expand" -> expandModules
        "indent" -> indent
        "glob-files" -> globFiles
        "tabular" -> return $ GlobalPragma $ PragmaOptTabular True
        "no-tabular" -> return $ GlobalPragma $ PragmaOptTabular False
        "fragment" -> fragment
        _ -> fail $ "Unknown pragma " ++ t

    expandModules :: C.ParsecParser Pragma
    expandModules = do
      C.spaces
      dir <- C.parsecToken
      mns <- C.many (C.space *> C.spaces *> C.char '-' *> C.parsec)
      return $ FieldPragma $ PragmaExpandModules dir mns

    indent :: C.ParsecParser Pragma
    indent = do
      C.spaces
      GlobalPragma . PragmaOptIndent <$> C.integral

    fragment :: C.ParsecParser Pragma
    fragment = do
      C.spaces
      FieldPragma . PragmaFragment <$> C.parsecToken

    globFiles :: C.ParsecParser Pragma
    globFiles = do
      C.spaces
      t <- C.parsecToken
      case parseGlob t of
        Right g -> return $ FieldPragma $ PragmaGlobFiles g
        Left e -> C.unexpected e

stripWhitespace :: BS.ByteString -> BS.ByteString
stripWhitespace bs = case BS.uncons bs of
  Nothing -> bs
  Just (w, bs')
    | w == 32 -> stripWhitespace bs'
    | otherwise -> bs

parsePragmas :: Comments -> ([String], [Pragma])
parsePragmas = fmap Maybe.catMaybes . Either.partitionEithers . map parsePragma . unComments
