{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalGild.Refactoring where

import CabalGild.Fields.SourceFiles
import CabalGild.Monad
import CabalGild.Refactoring.ExpandExposedModules
import CabalGild.Refactoring.Fragments
import CabalGild.Refactoring.GlobFiles
import CabalGild.Refactoring.Type
import qualified Distribution.Fields as C

-------------------------------------------------------------------------------
-- Refactorings
-------------------------------------------------------------------------------

refactor :: forall m r. (MonadCabalGild r m) => [C.Field CommentsPragmas] -> m [C.Field CommentsPragmas]
refactor = rewriteFields rewrite
  where
    rewrite :: C.Field CommentsPragmas -> m (Maybe (C.Field CommentsPragmas))
    rewrite f@(C.Field (C.Name _ n) _)
      | n == "exposed-modules" || n == "other-modules" =
          combine
            [ refactoringFragments,
              refactoringExpandExposedModules
            ]
            f
      | n `elem` fileFields =
          combine
            [ refactoringFragments,
              refactoringGlobFiles
            ]
            f
      | otherwise =
          combine
            [ refactoringFragments
            ]
            f
    rewrite f@C.Section {} =
      combine
        [ refactoringFragments
        ]
        f

-- | Try refactorings in turn,
-- considering it done if one applies.
combine ::
  (Monad m) =>
  [C.Field CommentsPragmas -> m (Maybe (C.Field CommentsPragmas))] ->
  C.Field CommentsPragmas ->
  m (Maybe (C.Field CommentsPragmas))
combine [] _ = return Nothing
combine (r : rs) f = do
  m <- r f
  case m of
    Nothing -> combine rs f
    Just f' -> return (Just f')
