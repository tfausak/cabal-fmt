module CabalGild.Unstable.Exception.NoCabalFileFound where

import qualified Control.Monad.Catch as Exception

-- | This exception is thrown when no input or output file has been specified, stdin is a terminal device, and there is no cabal file in the current directory.
data NoCabalFileFound
  = NoCabalFileFound
  deriving (Eq, Show)

instance Exception.Exception NoCabalFileFound where
  displayException = const "No cabal file found the the current directory"
