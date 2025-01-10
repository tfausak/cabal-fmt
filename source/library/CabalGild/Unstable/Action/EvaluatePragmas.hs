module CabalGild.Unstable.Action.EvaluatePragmas where

import qualified CabalGild.Unstable.Action.EvaluatePragmas.Discover as EvaluateDiscover
import qualified CabalGild.Unstable.Action.EvaluatePragmas.Version as EvaluateVersion
import qualified CabalGild.Unstable.Class.MonadWalk as MonadWalk
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified Control.Monad.Catch as Exception
import qualified Distribution.Fields as Fields

-- | High level wrapper around 'field' that makes this action easier to compose
-- with other actions.
run ::
  (Exception.MonadThrow m, MonadWalk.MonadWalk m, Show p, Show q) =>
  FilePath ->
  ([Fields.Field (p, [Comment.Comment q])], cs) ->
  m ([Fields.Field (p, [Comment.Comment q])], cs)
run p (fs, cs) = (,) <$> traverse (EvaluateDiscover.field p . EvaluateVersion.field) fs <*> pure cs
