module Handlers where

import GameState
import Player
import Generics

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Control.Lens

import qualified Data.Set as S
import qualified Data.List as L


{-
    Utility functions
-}
keyToDir :: SpecialKey -> Direction
keyToDir KeyUp = DirUp
keyToDir KeyDown = DirDown
keyToDir KeyRight = DirRight
keyToDir KeyLeft = DirLeft

convertToArrow :: Key -> Key
convertToArrow (Char 'a') = SpecialKey KeyLeft
convertToArrow (Char 'd') = SpecialKey KeyRight
convertToArrow (Char 'w') = SpecialKey KeyUp
convertToArrow (Char 's') = SpecialKey KeyDown

arrowKeys = map SpecialKey [KeyUp, KeyDown, KeyLeft, KeyRight]
wasd = map Char ['w', 'a', 's', 'd']

{-----------------------------------------------------------------}

-- | Main event handler, distributes according to the KeyState
eventHandler :: Event -> GameState -> GameState
eventHandler e@(EventKey _ Down _ _) gs = handleDown e gs
eventHandler e@(EventKey _ Up _ _) gs = handleUp e gs
eventHandler (EventMotion (x,y)) gs = handleMouseMovement (x,y) gs
eventHandler _ gs = gs

-- | Handler for mouse movements.
handleMouseMovement :: (Float,Float) -> GameState -> GameState
handleMouseMovement (x,y) = player %~ rotatePlayer (x,y)

-- | Handler for KeyDown events.
handleDown :: Event -> GameState -> GameState
handleDown (EventKey key _ _ _) gs | key `L.elem` arrowKeys = handleArrowDown key gs
                                   | key `L.elem` wasd = handleArrowDown (convertToArrow key) gs
                                   | key == MouseButton LeftButton = addShot gs
                                   | otherwise = gs

-- | Handler for KeyUp events.
handleUp :: Event -> GameState -> GameState
handleUp (EventKey key _ _ _) gs | key `L.elem` arrowKeys = handleArrowUp key gs
                                 | key `L.elem` wasd = handleArrowUp (convertToArrow key) gs
                                 | otherwise = gs

-- | Handler for arrow down events.
handleArrowDown :: Key -> GameState -> GameState
handleArrowDown (SpecialKey key) = dirs %~ S.insert (keyToDir key)

-- | Handler for arrow up events.
handleArrowUp :: Key -> GameState -> GameState
handleArrowUp (SpecialKey key) = dirs %~ S.delete (keyToDir key)
