{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Login 
  ( LoginApi
  , loginHandler ) where

import Text.Blaze (ToMarkup)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Htmx as HX
import Servant
  ( Get,
    (:>),
  )
import Servant.HTML.Blaze (HTML)
import Monad (StonesHandler)

data Login = Login

instance ToMarkup Login where
  toMarkup _ =
    H.form 
    $ H.input H.! A.type_ "text"

type LoginApi = "login" :> Get '[HTML] Login

loginHandler :: StonesHandler Login
loginHandler = pure Login

