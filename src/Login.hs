{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Login 
  ( LoginApi
  , loginHandler ) where

import Lucid
import Servant
  ( Get,
    (:>),
  )
import Servant.HTML.Lucid (HTML)
import Monad (StonesHandler)

data Login = Login

instance ToHtml Login where
  toHtml _ =
    form_ $ do
      input_ [type_ "text"]

type LoginApi = "login" :> Get '[HTML] Login

loginHandler :: StonesHandler Login
loginHandler = pure Login

