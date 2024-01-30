{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalGild.Fields where

import qualified Data.Map.Strict as Map
import qualified Distribution.Compat.Newtype as Newtype
import qualified Distribution.FieldGrammar as C
import qualified Distribution.Fields.Field as C
import qualified Distribution.Parsec as C
import qualified Distribution.Pretty as C
import qualified Text.PrettyPrint as PP

-------------------------------------------------------------------------------
-- FieldDescr variant
-------------------------------------------------------------------------------

-- strict pair
data SP where
  FreeText :: SP
  SP ::
    !(f -> PP.Doc) ->
    !(forall m. (C.CabalParsing m) => m f) ->
    SP

-- | Lookup both pretty-printer and value parser.
--
-- As the value of the field is unknown, we have to work with it universally.
fieldDescrLookup ::
  (C.CabalParsing m) =>
  FieldDescrs s a ->
  C.FieldName ->
  r -> -- field is freetext
  (forall f. m f -> (f -> PP.Doc) -> r) ->
  Maybe r
fieldDescrLookup (F m) fn ft kont = kont' <$> Map.lookup fn m
  where
    kont' (SP a b) = kont b a
    kont' FreeText = ft

-- | A collection field parsers and pretty-printers.
newtype FieldDescrs s a = F {runF :: Map.Map C.FieldName SP}
  deriving (Functor)

coerceFieldDescrs :: FieldDescrs s a -> FieldDescrs () ()
coerceFieldDescrs (F a) = F a

instance Semigroup (FieldDescrs s a) where
  F a <> F b = F (a <> b)

instance Monoid (FieldDescrs s a) where
  mempty = F Map.empty
  mappend = (<>)

instance Applicative (FieldDescrs s) where
  pure _ = F mempty
  f <*> x = F (mappend (runF f) (runF x))

singletonF ::
  C.FieldName ->
  (f -> PP.Doc) ->
  (forall m. (C.CabalParsing m) => m f) ->
  FieldDescrs s a
singletonF fn f g = F $ Map.singleton fn (SP f g)

instance C.FieldGrammar PrettyParsec FieldDescrs where
  blurFieldGrammar _ (F m) = F m

  booleanFieldDef fn _ _def = singletonF fn f C.parsec
    where
      f :: Bool -> PP.Doc
      f s = PP.text (show s)

  uniqueFieldAla fn _pack _ =
    singletonF fn (C.pretty . Newtype.pack' _pack) (Newtype.unpack' _pack <$> C.parsec)

  optionalFieldAla fn _pack _ =
    singletonF fn (C.pretty . Newtype.pack' _pack) (Newtype.unpack' _pack <$> C.parsec)

  optionalFieldDefAla fn _pack _ def =
    singletonF fn f (Newtype.unpack' _pack <$> C.parsec)
    where
      f s
        | s == def = PP.empty
        | otherwise = C.pretty (Newtype.pack' _pack s)

  monoidalFieldAla fn _pack _ =
    singletonF fn (C.pretty . Newtype.pack' _pack) (Newtype.unpack' _pack <$> C.parsec)

  freeTextField fn _ = F $ Map.singleton fn FreeText
  freeTextFieldDef fn _ = F $ Map.singleton fn FreeText
  freeTextFieldDefST fn _ = F $ Map.singleton fn FreeText

  prefixedFields _fnPfx _l = F mempty
  knownField _ = pure ()
  deprecatedSince _ _ x = x
  removedIn _ _ x = x
  availableSince _ _ = id
  hiddenField _ = F mempty

class (C.Pretty a, C.Parsec a) => PrettyParsec a

instance (C.Pretty a, C.Parsec a) => PrettyParsec a
