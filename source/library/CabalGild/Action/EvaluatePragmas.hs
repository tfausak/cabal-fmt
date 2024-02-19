module CabalGild.Action.EvaluatePragmas where

import qualified CabalGild.Class.MonadWalk as MonadWalk
import qualified CabalGild.Extra.ModuleName as ModuleName
import qualified CabalGild.Extra.Name as Name
import qualified CabalGild.Extra.String as String
import qualified CabalGild.Type.Comment as Comment
import qualified CabalGild.Type.Pragma as Pragma
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Utils.Generic as Utils
import qualified System.FilePath as FilePath

-- | High level wrapper around 'fields' that makes this action easier to
-- compose with other actions.
run ::
  (MonadWalk.MonadWalk m) =>
  FilePath ->
  ([Fields.Field [Comment.Comment a]], cs) ->
  m ([Fields.Field [Comment.Comment a]], cs)
run p (fs, cs) = (,) <$> fields p fs <*> pure cs

-- | Evaluates pragmas modules within the given fields.
fields ::
  (MonadWalk.MonadWalk m) =>
  FilePath ->
  [Fields.Field [Comment.Comment a]] ->
  m [Fields.Field [Comment.Comment a]]
fields = mapM . field

-- | Evaluates pragmas within the given field. Or, if the field is a section,
-- evaluates pragmas recursively within the fields of the section.
--
-- If modules are discovered for a field, that fields lines are completely
-- replaced. If anything goes wrong while discovering modules, the original
-- field is returned.
field ::
  (MonadWalk.MonadWalk m) =>
  FilePath ->
  Fields.Field [Comment.Comment a] ->
  m (Fields.Field [Comment.Comment a])
field p f = case f of
  Fields.Field n _ -> fmap (Maybe.fromMaybe f) . MaybeT.runMaybeT $ do
    Monad.guard $ Set.member (Name.value n) relevantFieldNames
    c <- hoistMaybe . Utils.safeLast $ Name.annotation n
    x <- hoistMaybe . Parsec.simpleParsecBS $ Comment.value c
    y <- case x of
      Pragma.Discover y -> pure y
    let d = FilePath.combine (FilePath.takeDirectory p) y
    fs <- Trans.lift $ MonadWalk.walk d
    pure
      . Fields.Field n
      . fmap (ModuleName.toFieldLine [])
      . Maybe.mapMaybe (ModuleName.fromFilePath . FilePath.makeRelative d)
      $ Maybe.mapMaybe (stripAnyExtension extensions) fs
  Fields.Section n sas fs -> Fields.Section n sas <$> fields p fs

-- | These are the names of the fields that can have this action applied to
-- them.
relevantFieldNames :: Set.Set Fields.FieldName
relevantFieldNames =
  Set.fromList $
    fmap
      String.toUtf8
      [ "exposed-modules",
        "other-modules"
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
      "lhs",
      "ly",
      "x",
      "y"
    ]

-- | This was added in @transformers-0.6.0.0@. See
-- <https://hub.darcs.net/ross/transformers/issue/49>.
hoistMaybe :: (Applicative f) => Maybe a -> MaybeT.MaybeT f a
hoistMaybe = MaybeT.MaybeT . pure