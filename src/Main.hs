module Main (main) where

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Proxy (..),
    serve,
  )
import Servant.Server
  ( Application,
    hoistServer,
  )

import Monad (StonesEnv(..), runStones)
import Stone (Stone(White))
import Api (Api, api)

proxy :: Proxy Api
proxy = Proxy

app :: StonesEnv -> Application
app stonesEnv = serve proxy $ hoistServer proxy (runStones stonesEnv) api

main :: IO ()
main = do
  turn <- atomically $ newTVar White
  run 8080 (app $ StonesEnv turn)
