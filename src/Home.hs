{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Home 
  ( HomeApi
  , homeHandler) where

import Lucid
import Servant (Get)
import Grid (nGrid, Grid)
import Servant.HTML.Lucid (HTML)
import Monad (StonesHandler)

data Home a = Home a

instance (ToHtml a) => ToHtml (Home a) where
  toHtml (Home a) = html_ $ do
    head_ $ do
      title_ "Stones"
      script_
        [src_ "https://unpkg.com/htmx.org@1.9.8"]
        ("" :: Html ())
      script_
        [src_ "https://cdn.tailwindcss.com"]
        ("" :: Html ())
    body_ [class_ "w-full h-full" ]$ do
        toHtml a
  toHtmlRaw _ = mempty

type HomeApi = Get '[HTML] (Home Grid)

homeHandler :: StonesHandler (Home Grid)
homeHandler = pure (Home (nGrid 19))
