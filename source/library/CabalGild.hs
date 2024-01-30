{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
--
-- This is a demo application of how you can make Cabal-like
-- file formatter.
module CabalGild where

import CabalGild.Comments
import CabalGild.Fields
import CabalGild.Fields.BuildDepends hiding (parse)
import CabalGild.Fields.Extensions hiding (parse)
import CabalGild.Fields.Modules hiding (parse)
import CabalGild.Fields.SourceFiles hiding (parse)
import CabalGild.Fields.TestedWith
import CabalGild.FreeText
import CabalGild.Monad
import CabalGild.Options
import CabalGild.Parser
import CabalGild.Pragma
import CabalGild.Refactoring
import CabalGild.Refactoring.Type (CommentsPragmas)
import Control.Monad (join)
import qualified Control.Monad.Except as Except
import Control.Monad.Reader (asks, local)
import qualified Data.ByteString as BS
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import Data.Function ((&))
import Data.Functor (($>))
import qualified Data.Maybe as Maybe
import qualified Distribution.CabalSpecVersion as C
import qualified Distribution.Compat.Lens as Lens
import qualified Distribution.FieldGrammar.Parsec as C
import qualified Distribution.Fields as C
import qualified Distribution.Fields.ConfVar as C
import qualified Distribution.Fields.Pretty as C
import qualified Distribution.PackageDescription.FieldGrammar as C
import qualified Distribution.Parsec as C
import qualified Distribution.Pretty as C
import qualified Distribution.Types.Condition as C
import qualified Distribution.Types.ConfVar as C
import qualified Distribution.Types.GenericPackageDescription as C
import qualified Distribution.Types.PackageDescription as C
import qualified Distribution.Types.VersionRange as C
import qualified Distribution.Utils.Generic as C
import qualified Text.PrettyPrint as PP

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

cabalGild :: (MonadCabalGild r m) => FilePath -> BS.ByteString -> m String
cabalGild filepath contents = do
  -- determine cabal-version
  cabalFile <- asks (optCabalFile . Lens.view options)
  csv <-
    if cabalFile
      then do
        gpd <- parseGpd filepath contents
        return $
          C.specVersion $
            C.packageDescription gpd
      else return C.cabalSpecLatest

  inputFields' <- parseFields contents
  let (inputFieldsC, endComments) = attachComments contents inputFields'

  -- parse pragmas
  let parse :: (MonadCabalGild r f) => (a, Comments) -> f (a, Comments, [Pragma])
      parse (pos, c) = case parsePragmas c of (ws, ps) -> Foldable.traverse_ displayWarning ws $> (pos, c, ps)
  inputFieldsP' <- traverse (traverse parse) inputFieldsC
  endCommentsPragmas <- case parsePragmas endComments of
    (ws, ps) -> Foldable.traverse_ displayWarning ws $> ps

  -- apply refactorings
  let inputFieldsP :: [C.Field CommentsPragmas]
      inputFieldsP = map (fmap (fmap (snd . partitionPragmas))) inputFieldsP'

  inputFieldsR <- refactor inputFieldsP

  -- options morphisms
  let pragmas :: [GlobalPragma]
      pragmas =
        fst $
          partitionPragmas $
            foldMap (foldMap $ \(_, _, z) -> z) inputFieldsP' <> endCommentsPragmas

      optsEndo :: OptionsMorphism
      optsEndo = foldMap pragmaToOM pragmas

  local (Lens.over options $ \o -> runOptionsMorphism optsEndo $ o {optSpecVersion = csv}) $ do
    indentWith <- asks (optIndent . Lens.view options)
    let inputFields = inputFieldsR

    outputPrettyFields <-
      genericFromParsecFields
        (\n (x, _, _) -> prettyFieldLines n x)
        prettySectionArgs
        inputFields

    return $
      C.showFields' (fromComments . (\(_, y, _) -> y)) (const id) indentWith outputPrettyFields
        & if nullComments endComments
          then id
          else (++ unlines ("" : [C.fromUTF8BS c | c <- unComments endComments]))

fromComments :: Comments -> C.CommentPosition
fromComments (Comments []) = C.NoComment
fromComments (Comments bss) = C.CommentBefore (map C.fromUTF8BS bss)

genericFromParsecFields ::
  (Applicative f) =>
  -- | transform field contents
  (C.FieldName -> ann -> [C.FieldLine ann] -> f PP.Doc) ->
  -- | transform section arguments
  (C.FieldName -> [C.SectionArg ann] -> f [PP.Doc]) ->
  [C.Field ann] ->
  f [C.PrettyField ann]
genericFromParsecFields f g = goMany
  where
    goMany = traverse go

    go (C.Field (C.Name ann name) fls) = C.PrettyField ann name <$> f name ann fls
    go (C.Section (C.Name ann name) secargs fs) = C.PrettySection ann name <$> g name secargs <*> goMany fs

-------------------------------------------------------------------------------
-- Field prettyfying
-------------------------------------------------------------------------------

prettyFieldLines :: (MonadCabalGild r m) => C.FieldName -> C.Position -> [C.FieldLine CommentsPragmas] -> m PP.Doc
prettyFieldLines fn pos fls =
  Maybe.fromMaybe (C.prettyFieldLines fn fls) <$> knownField fn pos fls

knownField :: (MonadCabalGild r m) => C.FieldName -> C.Position -> [C.FieldLine CommentsPragmas] -> m (Maybe PP.Doc)
knownField fn pos fls = do
  opts <- asks (Lens.view options)
  let v = optSpecVersion opts
  let ft = fieldlinesToFreeText v pos (fmap (fmap $ \(x, _, _) -> x) fls)
  let ft' = showFreeText v ft

  return $ join $ fieldDescrLookup (fieldDescrs opts) fn (Just ft') $ \p pp ->
    case C.runParsecParser' v p "<input>" (C.fieldLinesToStream fls) of
      Right x -> Just (pp x)
      Left _ -> Nothing

fieldDescrs :: Options -> FieldDescrs () ()
fieldDescrs opts =
  buildDependsF opts
    <> buildToolDependsF opts
    <> setupDependsF opts
    <> defaultExtensionsF
    <> otherExtensionsF
    <> exposedModulesF
    <> otherModulesF
    <> testedWithF opts
    <> mconcat sourceFilesF
    <> coerceFieldDescrs C.packageDescriptionFieldGrammar
    <> coerceFieldDescrs C.buildInfoFieldGrammar

-------------------------------------------------------------------------------
-- Sections
-------------------------------------------------------------------------------

prettySectionArgs :: (MonadCabalGild r m) => C.FieldName -> [C.SectionArg ann] -> m [PP.Doc]
prettySectionArgs x args =
  prettySectionArgs' x args `Except.catchError` \_ ->
    return (C.prettySectionArgs x args)

prettySectionArgs' :: (MonadCabalGild r m) => a -> [C.SectionArg ann] -> m [PP.Doc]
prettySectionArgs' _ args = do
  c <- runParseResult "<args>" "" $ C.parseConditionConfVar (map (C.zeroPos <$) args)
  return [ppCondition c]

-------------------------------------------------------------------------------
-- PrettyPrint condition
-------------------------------------------------------------------------------

-- This is originally from Cabal

ppCondition :: C.Condition C.ConfVar -> PP.Doc
ppCondition (C.Var x) = ppConfVar x
ppCondition (C.Lit b) = PP.text (show b)
ppCondition (C.CNot c) = PP.char '!' PP.<> ppCondition c
ppCondition (C.COr c1 c2) = PP.parens (PP.hsep [ppCondition c1, PP.text "||", ppCondition c2])
ppCondition (C.CAnd c1 c2) = PP.parens (PP.hsep [ppCondition c1, PP.text "&&", ppCondition c2])

ppConfVar :: C.ConfVar -> PP.Doc
ppConfVar (C.OS os) = PP.text "os" PP.<> PP.parens (C.pretty os)
ppConfVar (C.Arch arch) = PP.text "arch" PP.<> PP.parens (C.pretty arch)
ppConfVar (C.PackageFlag name) = PP.text "flag" PP.<> PP.parens (C.pretty name)
ppConfVar (C.Impl c v)
  | v == C.anyVersion = PP.text "impl" PP.<> PP.parens (C.pretty c)
  | otherwise = PP.text "impl" PP.<> PP.parens (C.pretty c PP.<+> C.pretty v)

-------------------------------------------------------------------------------
-- Pragma to OM
-------------------------------------------------------------------------------

partitionPragmas :: [Pragma] -> ([GlobalPragma], [FieldPragma])
partitionPragmas = Either.partitionEithers . map p
  where
    p (GlobalPragma x) = Left x
    p (FieldPragma x) = Right x

pragmaToOM :: GlobalPragma -> OptionsMorphism
pragmaToOM (PragmaOptIndent n) = mkOptionsMorphism $ \opts -> opts {optIndent = n}
pragmaToOM (PragmaOptTabular b) = mkOptionsMorphism $ \opts -> opts {optTabular = b}
