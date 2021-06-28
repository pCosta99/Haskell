module Bullet where

import Generics

import Control.Lens.TH
import Control.Lens

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Geometry.Angle

data Bullet = Bullet {
    _direction :: Vector,
    _velocity :: Vector,
    _position :: Position
} deriving (Show)

instance Tickable Bullet where
    tick t (Bullet d@(dx,dy) v@(vx,vy) (Pos x y)) = Bullet d v (Pos (x + (vx*t*dx)) (y - (vy*t*dy)))

instance ToPicture Bullet where
    toPicture (Bullet dv _ (Pos x y)) = translate x y $ rotate (radToDeg $ argV dv) $ polygon [(0,10),(20,10),(20,0),(0,0)]
