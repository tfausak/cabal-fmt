-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalGild.Options
  ( Options (..),
    defaultOptions,
    OptionsMorphism,
    mkOptionsMorphism,
    runOptionsMorphism,
    HasOptions (..),
  )
where

import qualified CabalGild.Type.Mode as Mode
import qualified Distribution.CabalSpecVersion as C
import Distribution.Compat.Lens (LensLike')

data Options = Options
  { optError :: !Bool,
    optIndent :: !Int,
    optTabular :: !Bool,
    optCabalFile :: !Bool,
    optSpecVersion :: !C.CabalSpecVersion,
    optMode :: !Mode.Mode,
    optStdinInputFile :: !(Maybe FilePath)
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  Options
    { optError = False,
      optIndent = 2,
      optTabular = True,
      optCabalFile = True,
      optSpecVersion = C.cabalSpecLatest,
      optMode = Mode.Stdout,
      optStdinInputFile = Nothing
    }

newtype OptionsMorphism = OM (Options -> Options)

runOptionsMorphism :: OptionsMorphism -> Options -> Options
runOptionsMorphism (OM f) = f

mkOptionsMorphism :: (Options -> Options) -> OptionsMorphism
mkOptionsMorphism = OM

instance Semigroup OptionsMorphism where
  OM f <> OM g = OM (g . f)

instance Monoid OptionsMorphism where
  mempty = OM id
  mappend = (<>)

class HasOptions e where
  options :: (Functor f) => LensLike' f e Options

instance HasOptions Options where
  options = id
