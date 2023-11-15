{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Home 
  ( HomeApi
  , homeHandler) where

import Text.Blaze (ToMarkup)
import Text.Blaze.Html (toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Servant (Get)
import Grid (nGrid, Grid)
import Servant.HTML.Blaze (HTML)
import Monad (StonesHandler)

data Home a = Home a

instance (ToMarkup a) => ToMarkup (Home a) where
  toMarkup (Home a) = H.docTypeHtml $ do
    H.head $ do
      H.title "Stones"
      H.script H.! A.src "https://unpkg.com/htmx.org@1.9.8" $ mempty
      H.script H.! A.src "https://cdn.tailwindcss.com" $ mempty
    H.body $ toHtml a

type HomeApi = Get '[HTML] (Home Grid)

homeHandler :: StonesHandler (Home Grid)
homeHandler = pure (Home (nGrid 19))
