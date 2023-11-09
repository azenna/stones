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
import Lucid
import Lucid.Htmx
import Servant
  ( Post,
    (:>),
  )
import Servant.HTML.Lucid (HTML)
import Monad (StonesHandler, turn)
import Stone (Stone(..), next)

newtype Square = Square {unSquare :: Maybe Stone}

instance ToHtml Square where
  toHtml s =
    div_
      [ class_ $
          "w-14 h-14 border "
            <> fromMaybe
              "bg-neutral-500"
              (color <$> unSquare s),
        hxPost_ "/square",
        hxTrigger_ "click",
        hxSwap_ "outerHTML"
      ]
      mempty
    where
      color Black = "bg-neutral-950"
      color White = "bg-neutral-50"
  toHtmlRaw _ = mempty

type SquareApi = "square" :> Post '[HTML] Square

squareHandler :: StonesHandler Square
squareHandler = do
  t <- turn <$> ask
  s <- liftIO $ atomically $ do
    x <- TV.readTVar t
    TV.writeTVar t $ next x
    pure x
  pure (Square $ Just s)
