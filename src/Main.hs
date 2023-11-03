{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lucid
import Lucid.Htmx
import Servant ( Proxy(..)
               , Post
               , Get
               , Raw
               , serve
               , (:<|>)(..)
               , (:>)
               , serveDirectoryWebApp)
import Servant.Server (Handler, Server)
import Servant.HTML.Lucid (HTML)
import Network.Wai.Handler.Warp (run)

data Square = Black | White | Empty

instance ToHtml Square where
  toHtml s = 
    div_ 
      [ class_ $ "w-14 h-14 border " <> color s
      , hxPost_ "/black"
      , hxTrigger_ "click"
      , hxSwap_ "outerHTML" ] 
      mempty
    where color Black = "bg-neutral-950"
          color White = "bg-neutral-50"
          color Empty = "bg-neutral-400"
  toHtmlRaw _ = mempty

newtype Grid = Grid [[Square]]

nGrid :: Int -> Grid
nGrid n = Grid $ (take n . repeat) $ take n $ repeat Empty

myGrid = nGrid 19

instance ToHtml Grid where
  toHtml (Grid g) = div_ $ do
    mapM_ (div_ [class_ "flex"]. mapM_ toHtml) g
  
data Home = Home

instance ToHtml Home where
  toHtml _ = html_ $ do
    head_ $ do
      title_ "Stones"
      useHtmx
      script_
        [src_ "https://cdn.tailwindcss.com"]
        ("" :: Html ())
    body_ $ do 
      p_ [class_ "bg-blue-300"] "hello world"
      toHtml myGrid
  toHtmlRaw _ = mempty

homeHandler :: Handler Home
homeHandler = pure Home

type SquareEnd = "black" :> Post '[HTML] Square

squareHandler :: Handler Square
squareHandler = pure Black

type API =
  Get '[HTML] Home 
  :<|> SquareEnd 
  :<|> Raw

stones :: Server API
stones =
  homeHandler 
  :<|> squareHandler
  :<|> serveDirectoryWebApp "public"

main :: IO ()
main = run 8080 (serve (Proxy :: Proxy API) stones)
