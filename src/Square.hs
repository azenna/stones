{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Square
  ( SquareApi
  , squareHandler
  , Square(..)) where

import Data.Maybe (fromMaybe)
import qualified Control.Concurrent.STM.TVar as TV
import Control.Monad.Reader (liftIO, ask)
import Control.Monad.STM (atomically)
import Text.Blaze (ToMarkup)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Htmx as HX
import Servant
  ( Post,
    (:>),
  )
import Servant.HTML.Blaze (HTML)
import Monad (StonesHandler, turn)
import Stone (Stone(..), next)

newtype Square = Square {unSquare :: Maybe Stone}

instance ToMarkup Square where
  toMarkup s =
    H.div 
      H.! (A.class_ 
        $  "w-14 h-14 border "
        <> fromMaybe "bg-neutral-500" (color <$> unSquare s))
      H.! HX.hxPost "/square"
      H.! HX.hxTrigger "click"
      H.! HX.hxSwap "outerHTML"
      $ mempty
    where  
      color Black = "bg-neutral-950"
      color White = "bg-neutral-50"

type SquareApi = "square" :> Post '[HTML] Square

squareHandler :: StonesHandler Square
squareHandler = do
  t <- turn <$> ask
  s <- liftIO $ atomically $ do
    x <- TV.readTVar t
    TV.writeTVar t $ next x
    pure x
  pure (Square $ Just s)
