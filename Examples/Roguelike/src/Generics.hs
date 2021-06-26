{-# LANGUAGE TemplateHaskell #-}
module Generics where

import Graphics.Gloss (Picture)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Control.Lens.TH

data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Show, Eq, Ord, Enum)

data Position = Pos {
    _x :: Float,
    _y :: Float
} deriving (Show)

makeLenses ''Position

addPos :: Position -> Position -> Position
addPos (Pos x1 y1) (Pos x2 y2) = Pos (x1+x2) (y1+y2)

mulPos :: Position -> Position -> Position
mulPos (Pos x1 y1) (Pos x2 y2) = Pos (x1*x2) (y1*y2)

class ToPicture a where
    toPicture :: a -> Picture

class Tickable a where
    tick :: Float -> a -> a

-- | List return.
singl x = [x]

-- | Transform a point into a Position.
p2Pos :: Point -> Position
p2Pos = uncurry Pos

-- | Transform a position into a point.
pos2P :: Position -> Point
pos2P (Pos x y) = (x,y)
