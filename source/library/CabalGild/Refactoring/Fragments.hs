{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalGild.Refactoring.Fragments
  ( refactoringFragments,
  )
where

import CabalGild.Comments
import CabalGild.Monad
import CabalGild.Parser
import CabalGild.Pragma
import CabalGild.Refactoring.Type
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Distribution.Fields as C
import qualified Distribution.Fields.Field as C
import qualified Distribution.Fields.Pretty as C
import qualified Distribution.Parsec as C
import Distribution.Utils.Generic (fromUTF8BS)
import Text.PrettyPrint (hsep, render)

-- | Expand fragments.
--
-- Applies to all fields and sections
refactoringFragments :: FieldRefactoring
refactoringFragments field = do
  mp <- parse (getPragmas field)
  case mp of
    Nothing -> pure Nothing
    Just p ->
      readFileBS p >>= \mcontents -> case mcontents of
        NoIO -> pure Nothing
        IOError err -> do
          displayWarning $ "Fragment " ++ p ++ " failed to read: " ++ show err
          pure Nothing
        Contents c -> do
          fields <- parseFields c
          case (field, fields) of
            (_, []) -> do
              displayWarning $ "Fragment " ++ p ++ " is empty."
              pure Nothing
            (C.Field (C.Name _ n) _, C.Section name@(C.Name _ _) arg _ : _) -> do
              displayWarning $ "Fragment " ++ p ++ " contains a section " ++ showSection name arg ++ ", expecting field " ++ show n ++ "."
              pure Nothing
            (C.Section name@(C.Name _ _) arg _, C.Field (C.Name _ n') _ : _) -> do
              displayWarning $ "Fragment " ++ p ++ " contains a field " ++ show n' ++ ", expecting section " ++ showSection name arg ++ "."
              pure Nothing
            (C.Field name@(C.Name _ n) _, C.Field (C.Name _ n') fls' : rest) -> do
              Monad.unless (null rest) $
                displayWarning $
                  "Fragment " ++ p ++ " contains multiple fields or sections, using only the first."
              if n == n'
                then do
                  -- everything is fine, replace
                  pure (Just (C.Field name (noCommentsPragmas fls')))
                else do
                  displayWarning $ "Fragment " ++ p ++ " contains field " ++ show n' ++ ", expecting field " ++ show n ++ "."
                  pure Nothing
            (C.Section name@(C.Name _ _) arg _, C.Section name'@(C.Name _ _) arg' fs' : rest) -> do
              Monad.unless (null rest) $
                displayWarning $
                  "Fragment " ++ p ++ " contains multiple fields or sections, using only the first."

              if Monad.void name == Monad.void name' && map Monad.void arg == map Monad.void arg'
                then do
                  pure (Just (C.Section name arg (noCommentsPragmas fs')))
                else do
                  displayWarning $ "Fragment " ++ p ++ " contains a section " ++ showSection name arg ++ ", expecting section " ++ showSection name' arg' ++ "."
                  pure Nothing
  where
    noCommentsPragmas :: (Functor f) => [f ann] -> [f CommentsPragmas]
    noCommentsPragmas = map ((C.zeroPos, Comments [], []) <$)

    getPragmas :: C.Field CommentsPragmas -> [FieldPragma]
    getPragmas = (\(_, _, z) -> z) . C.fieldAnn

    showSection :: C.Name ann -> [C.SectionArg ann] -> String
    showSection (C.Name _ n) [] = show n
    showSection (C.Name _ n) args = show (fromUTF8BS n ++ " " ++ render (hsep (C.prettySectionArgs n args)))

    parse :: (MonadCabalGild r m) => [FieldPragma] -> m (Maybe FilePath)
    parse = fmap Foldable.asum . traverse go
      where
        go :: (Monad m) => FieldPragma -> m (Maybe FilePath)
        go (PragmaFragment f) = return (Just f)
        go _ = return Nothing
