{-# LANGUAGE TemplateHaskell #-}
module Character where

import Generics

import Control.Lens.TH
import Control.Lens

import Graphics.Gloss.Data.Color
import Graphics.Gloss (Color)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector

{-
 - Awful design makes it basically impossible to align all this shit
 - -}

-- | For now, every Body is a circle
data Body = Body {
    _b_size :: Float,
    _b_center :: Point, -- ^ We only keep the center for the sake of having this info locally
    _b_color :: Color
} deriving (Show)

makeLenses ''Body

instance ToPicture Body where
    toPicture (Body s _ clr) = color clr $ polygon $ rectanglePath s s

-- | Weapons are pipes that come out of the body
data Weapon = Weapon {
    _w_size :: Float,
    _w_tip :: Point -- ^ Useful to know where the bullets will spawn
} deriving (Show)

makeLenses ''Weapon

instance ToPicture Weapon where
    toPicture (Weapon s c) = color (greyN 0.5) $ polygon $ rectanglePath s s

data Character = CHR {
    _body :: Body,
    _weapon :: Weapon,
    _c_dir :: Vector
} deriving (Show)

makeLenses ''Character

instance ToPicture Character where
    toPicture (CHR body weapon (dvx,_)) = pictures [toPicture body, translate (body^.b_size / 2) 0 $ toPicture weapon]

createCharacter :: Float -> Character
createCharacter size = CHR (Body size (0,0) black) (Weapon (size/2) (3*size/5,0)) (0,0)

updateVD :: Vector -> Character -> Character
updateVD vd = c_dir .~ (normalizeV vd)

-- | Returns the center of the weapon.
getWeaponTip :: Character -> Point
getWeaponTip (CHR _ (Weapon _ (x,y)) (dx,dy)) = (x*dx,y)
