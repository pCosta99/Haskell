{-# LANGUAGE TemplateHaskell #-}
module Player where

import Generics
import Character
import Bullet

import Control.Lens.TH
import Control.Lens

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Color
import Graphics.Gloss (Color)
import Graphics.Gloss.Data.Picture

data Player = P {
    _p_pos :: Position,
    _p_char :: Character,
    _dir_vec :: Vector
} deriving (Show)

makeLenses ''Player

instance ToPicture Player where
    toPicture p@(P (Pos x y) chr dv) = translate x y $ rotate (radToDeg $ argV dv) $ toPicture chr

-- | Moves the player in a direction.
movePlayer :: Direction -> Player -> Player
movePlayer dir p = case dir of DirRight -> p_pos %~ x %~ (+5) $ p
                               DirLeft  -> p_pos %~ x %~ (flip (-) 5) $ p
                               DirDown  -> p_pos %~ y %~ (flip (-) 5) $ p
                               DirUp    -> p_pos %~ y %~ (+5) $ p

-- | Rotates the player according to the mouse motion.
rotatePlayer :: Vector -> Player -> Player
rotatePlayer vd@(x,y) = (p_char %~ (updateVD (x,-y))) . (dir_vec .~ (x,-y))

shoot :: Player -> Bullet
shoot (P pos char dv) = Bullet (normalizeV dv) (500,500) (pos `addPos` weapCenter) where
    weapCenter = p2Pos $ getWeaponTip char
