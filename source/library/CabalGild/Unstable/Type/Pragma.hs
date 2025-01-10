module CabalGild.Unstable.Type.Pragma where

import qualified Control.Monad as Monad
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.Parsec as Parsec

-- | A pragma, which is a special comment used to customize behavior.
newtype Pragma a = Pragma a
  deriving (Eq, Show)

instance (Parsec.Parsec a) => Parsec.Parsec (Pragma a) where
  parsec = do
    CharParsing.spaces
    Monad.void $ CharParsing.string cabalGildPragmaPrefix
    CharParsing.spaces
    Pragma <$> Parsec.parsec

cabalGildPragmaPrefix :: String
cabalGildPragmaPrefix = "cabal-gild:"
