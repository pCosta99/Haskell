{-# LANGUAGE TemplateHaskell #-}
module GameState where

import Player
import Generics
import Bullet
import Character

import qualified Data.Set as S

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector

import Control.Lens.TH
import Control.Lens

data GameState = GS {
    _player :: Player,
    _dirs :: S.Set Direction, -- ^ the directions in which we are moving atm
    _bullets :: [Bullet] -- ^ Bullets ongoing
} deriving (Show)

makeLenses ''GameState

instance ToPicture GameState where
    toPicture = pictures . concat . ([playerPics, bulletsPics] <*>) . pure where
        playerPics = singl . toPicture . view player
        bulletsPics = map toPicture . view bullets

addShot :: GameState -> GameState
addShot gs = bullets %~ (shoot (gs^.player) :) $ gs
