module Monad 
  ( StonesHandler
  , StonesEnv(..)
  , runStones ) where

import Control.Monad.Reader (runReaderT, ReaderT)
import Control.Concurrent.STM.TVar (TVar)
import Servant.Server (Handler)
import Stone (Stone)

data StonesEnv = StonesEnv
  { turn :: TVar Stone }

type StonesHandler = ReaderT StonesEnv Handler

runStones :: StonesEnv -> StonesHandler a -> Handler a
runStones  = flip $ runReaderT
