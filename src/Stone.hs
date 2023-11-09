module Stone
  ( Stone(..)
  , next ) where

data Stone = Black | White deriving (Show)

next :: Stone -> Stone
next Black = White
next White = Black
