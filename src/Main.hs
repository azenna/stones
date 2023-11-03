{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}


module Main (main) where

import Control.Monad.Reader
  ( ReaderT
  , MonadIO
  , MonadReader
  , ask
  , runReaderT
  , liftIO)
import Control.Concurrent.STM.TVar
  ( TVar
  , newTVar
  , readTVar
  , writeTVar )
import Control.Monad.STM (atomically)
import Data.Maybe (fromMaybe)
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
import Servant.Server 
  ( Handler
  , Server
  , ServerT 
  , Application
  , hoistServer)
import Servant.HTML.Lucid (HTML)
import Network.Wai.Handler.Warp (run)

data Stone = Black | White

newtype Square = Square { unSquare :: Maybe Stone }

instance ToHtml Square where
  toHtml s = 
    div_ 
      [ class_ $ "w-14 h-14 border " <> 
          fromMaybe 
            "bg-neutral-500"
            (color <$> unSquare s) 
      , hxPost_ "/square"
      , hxTrigger_ "click"
      , hxSwap_ "outerHTML" ] 
      mempty
    where color Black = "bg-neutral-950"
          color White = "bg-neutral-50"
  toHtmlRaw _ = mempty

newtype Grid = Grid [[Square]]

nGrid :: Int -> Grid
nGrid n = Grid $ (take n . repeat) $ take n $ repeat (Square Nothing)

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

homeHandler :: StonesHandler Home
homeHandler = pure Home

type SquareEnd = "square" :> Post '[HTML] Square

squareHandler :: StonesHandler Square
squareHandler = do
    t <- turn <$> ask
    s <- liftIO $ atomically $ do 
      x <- readTVar t
      writeTVar t $ case x of
        White -> Black
        Black -> White
      pure x
    pure (Square $ Just s)

data Turn = Turn
  { turn :: TVar Stone }

type StonesHandler = ReaderT Turn Handler

type API =
  Get '[HTML] Home 
  :<|> SquareEnd 
  :<|> Raw

stones :: ServerT API StonesHandler
stones =
  homeHandler 
  :<|> squareHandler
  :<|> serveDirectoryWebApp "public"

nt :: Turn -> StonesHandler a -> Handler a
nt t x = runReaderT x t

api :: Proxy API
api = Proxy

app :: Turn -> Application
app t = serve api $ hoistServer api (nt t) stones

main :: IO ()
main = do
  initialTurn <- atomically $ newTVar White
  run 8080 (app $ Turn initialTurn)
