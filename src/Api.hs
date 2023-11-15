{-# LANGUAGE TypeOperators #-}

module Api
  ( Api
  , api ) where

import Servant
  ( Raw,
    serveDirectoryWebApp,
    (:<|>)(..),
  )
import Servant.Server (ServerT)
import Monad (StonesHandler)
import Grid (GridApi, gridHandler)
import Login (LoginApi, loginHandler)
import Square (SquareApi, squareHandler)
import Home (HomeApi, homeHandler)

type Api =
  HomeApi
    :<|> LoginApi
    :<|> GridApi
    :<|> SquareApi :<|> Raw

api :: ServerT Api StonesHandler
api =
  homeHandler
    :<|> loginHandler
    :<|> gridHandler
    :<|> squareHandler
    :<|> serveDirectoryWebApp "public"
