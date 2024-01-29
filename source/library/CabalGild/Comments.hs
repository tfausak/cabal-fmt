{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalGild.Comments where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Distribution.Fields as C
import qualified Distribution.Fields.Field as C
import qualified Distribution.Parsec as C

-------------------------------------------------------------------------------
-- Comments wrapper
-------------------------------------------------------------------------------

newtype Comments = Comments [BS.ByteString]
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

unComments :: Comments -> [BS.ByteString]
unComments (Comments cs) = cs

nullComments :: Comments -> Bool
nullComments (Comments cs) = null cs

-------------------------------------------------------------------------------
-- Attach comments
-------------------------------------------------------------------------------

-- | Returns a 'C.Field' forest with comments attached.
--
-- * Comments are attached to the field after it.
-- * A glitch: comments "inside" the field are attached to the field after it.
-- * End-of-file comments are returned separately.
attachComments ::
  -- | source with comments
  BS.ByteString ->
  -- | parsed source fields
  [C.Field C.Position] ->
  ([C.Field (C.Position, Comments)], Comments)
attachComments input inputFields =
  (overAnn attach attach' inputFields, endComments)
  where
    inputFieldsU :: [(FieldPath, C.Field C.Position)]
    inputFieldsU = fieldUniverseN inputFields

    comments :: [(Int, Comments)]
    comments = extractComments input

    comments' :: Map.Map FieldPath Comments
    comments' =
      Map.fromListWith
        (flip (<>))
        [ (path, cs)
          | (l, cs) <- comments,
            path <- Maybe.maybeToList (findPath C.fieldAnn l inputFieldsU)
        ]

    endComments :: Comments
    endComments =
      mconcat
        [ cs
          | (l, cs) <- comments,
            Maybe.isNothing (findPath C.fieldAnn l inputFieldsU)
        ]

    attach :: FieldPath -> C.Position -> (C.Position, Comments)
    attach fp pos = (pos, Maybe.fromMaybe mempty (Map.lookup fp comments'))

    attach' :: C.Position -> (C.Position, Comments)
    attach' pos = (pos, mempty)

overAnn :: forall a b. (FieldPath -> a -> b) -> (a -> b) -> [C.Field a] -> [C.Field b]
overAnn f h = go' id
  where
    go :: (FieldPath -> FieldPath) -> Int -> C.Field a -> C.Field b
    go g i (C.Field (C.Name a name) fls) =
      C.Field (C.Name b name) (h <$$> fls)
      where
        b = f (g (Nth i End)) a
    go g i (C.Section (C.Name a name) args fls) =
      C.Section (C.Name b name) (h <$$> args) (go' (g . Nth i) fls)
      where
        b = f (g (Nth i End)) a

    go' :: (FieldPath -> FieldPath) -> [C.Field a] -> [C.Field b]
    go' g = zipWith (go g) [0 ..]

    (<$$>) :: (Functor f1, Functor f2) => (x -> y) -> f1 (f2 x) -> f1 (f2 y)
    x <$$> y = (x <$>) <$> y

-------------------------------------------------------------------------------
-- Find comments in the input
-------------------------------------------------------------------------------

extractComments :: BS.ByteString -> [(Int, Comments)]
extractComments = go . zip [1 ..] . map (BS.dropWhileEnd isCR . BS.dropWhile isSpace8) . BS8.lines
  where
    go :: [(Int, BS.ByteString)] -> [(Int, Comments)]
    go [] = []
    go ((n, bs) : rest)
      | isComment bs = case span ((isComment .|| BS.null) . snd) rest of
          (h, t) -> (n, Comments $ bs : map snd h) : go t
      | otherwise = go rest

    (.||) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
    (f .|| g) x = f x || g x

    isSpace8 :: (Eq a, Num a) => a -> Bool
    isSpace8 w = w == 9 || w == 32

    isCR :: (Eq a, Num a) => a -> Bool
    isCR = (==) 13

    isComment :: BS.ByteString -> Bool
    isComment = BS.isPrefixOf "--"

-------------------------------------------------------------------------------
-- FieldPath
-------------------------------------------------------------------------------

-- | Paths input paths. Essentially a list of offsets. Own type ofr safety.
data FieldPath
  = End
  | Nth Int FieldPath -- nth field
  deriving (Eq, Ord, Show)

fieldUniverseN :: [C.Field ann] -> [(FieldPath, C.Field ann)]
fieldUniverseN = concat . zipWith g [0 ..]
  where
    g :: Int -> C.Field ann -> [(FieldPath, C.Field ann)]
    g n f' = [(Nth n p, f'') | (p, f'') <- fieldUniverse f']

fieldUniverse :: C.Field ann -> [(FieldPath, C.Field ann)]
fieldUniverse f@(C.Section _ _ fs) = (End, f) : concat (zipWith g [0 ..] fs)
  where
    g :: Int -> C.Field ann -> [(FieldPath, C.Field ann)]
    g n f' = [(Nth n p, f'') | (p, f'') <- fieldUniverse f']
fieldUniverse f@(C.Field _ _) = [(End, f)]

-- note: fieldUniverse* should produce 'FieldPath's in increasing order
-- that helps
findPath :: (a -> C.Position) -> Int -> [(FieldPath, a)] -> Maybe FieldPath
findPath _ _ [] = Nothing
findPath f l [(p, x)]
  | C.Position k _ <- f x =
      if l < k then Just p else Nothing
findPath f l ((_, x) : rest@((p, x') : _))
  | C.Position k _ <- f x,
    C.Position k' _ <- f x' =
      if k < l && l < k'
        then Just p
        else findPath f l rest
