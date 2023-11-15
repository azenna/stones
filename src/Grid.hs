{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Grid 
  ( GridApi
  , gridHandler
  , Grid
  , nGrid ) where

import Servant
  ( Get,
    (:>),
  )
import Text.Blaze (ToMarkup)
import Text.Blaze.Html (toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Servant.HTML.Blaze (HTML)
import Monad (StonesHandler)
import Square (Square(..))

newtype Grid = Grid [[Square]]

nGrid :: Int -> Grid
nGrid n = Grid $ (take n . repeat) $ take n $ repeat (Square Nothing)

instance ToMarkup Grid where
  toMarkup (Grid g) = H.div H.! A.class_ "w-full h-full"
    $ mapM_ ((H.div H.! A.class_ "flex w-full justify-center") . mapM_ toHtml) g

-- instance ToHtml Grid where
--   toHtml (Grid g) = div_ [class_ "w-full h-full"] $ do
--     mapM_ (div_ [class_ "flex w-full justify-center"] . mapM_ toHtml) g
--
--   toHtmlRaw _ = mempty


type GridApi = "grid" :> Get '[HTML] Grid

gridHandler :: StonesHandler Grid
gridHandler = pure (nGrid 19)
