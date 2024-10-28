module CabalGild.Unstable.Class.MonadHandle where

import qualified System.IO as System

class MonadHandle m where
  stdinIsTerminalDevice :: m Bool

instance MonadHandle IO where
  stdinIsTerminalDevice = System.hIsTerminalDevice System.stdin
