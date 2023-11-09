{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Grid 
  ( GridApi
  , gridHandler
  , Grid
  , nGrid ) where

import Lucid
import Servant
  ( Get,
    (:>),
  )
import Servant.HTML.Lucid (HTML)
import Monad (StonesHandler)
import Square (Square(..))

newtype Grid = Grid [[Square]]

nGrid :: Int -> Grid
nGrid n = Grid $ (take n . repeat) $ take n $ repeat (Square Nothing)

instance ToHtml Grid where
  toHtml (Grid g) = div_ [class_ "w-full h-full"]$ do
    mapM_ (div_ [class_ "flex w-full justify-center"] . mapM_ toHtml) g

  toHtmlRaw _ = mempty


type GridApi = "grid" :> Get '[HTML] Grid

gridHandler :: StonesHandler Grid
gridHandler = pure (nGrid 19)
