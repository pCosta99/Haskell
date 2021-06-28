module Main where

import GameState
import Player
import Bullet
import Generics
import Handlers
import Character

import Control.Lens

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import qualified Data.Set as S

iterator :: Float -> GameState -> GameState
iterator t gs = foldr (.) id changes gs where
    changes = updatePlayer ++ updateBullets
    updatePlayer = map (\d -> player %~ movePlayer d) $ S.toList $ gs^.dirs -- | Takes care of player movements
    updateBullets = [bullets %~ map (tick t)]

main :: IO ()
main = play FullScreen white 60 initState toPicture eventHandler iterator where
    window = InWindow "Nice Window" (200, 200) (10, 10)
    initState = GS (P (Pos 0 0) (createCharacter 50) (90,0)) S.empty []
