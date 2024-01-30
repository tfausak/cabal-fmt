{-# LANGUAGE RankNTypes #-}

-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalGild.Refactoring.Type where

import CabalGild.Comments
import CabalGild.Monad
import CabalGild.Pragma
import qualified Distribution.Fields as C
import qualified Distribution.Parsec as C

-------------------------------------------------------------------------------
-- Refactoring type
-------------------------------------------------------------------------------

type CommentsPragmas = (C.Position, Comments, [FieldPragma])

emptyCommentsPragmas :: CommentsPragmas
emptyCommentsPragmas = (C.zeroPos, mempty, mempty)

type FieldRefactoring =
  forall r m.
  (MonadCabalGild r m) =>
  (C.Field CommentsPragmas -> m (Maybe (C.Field CommentsPragmas)))

-------------------------------------------------------------------------------
-- Traversing refactoring
-------------------------------------------------------------------------------

-- | A top-to-bottom rewrite of sections and fields
rewriteFields ::
  (MonadCabalGild r m) =>
  (C.Field CommentsPragmas -> m (Maybe (C.Field CommentsPragmas))) ->
  [C.Field CommentsPragmas] ->
  m [C.Field CommentsPragmas]
rewriteFields f = goMany
  where
    goMany = traverse go

    go x = do
      m <- f x
      case m of
        Just y -> return y
        Nothing -> case x of
          C.Field {} -> return x
          C.Section name args fs -> C.Section name args <$> goMany fs
