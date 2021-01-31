module Kata.GetToTheChoppa where

import Data.List (reverse, (\\), minimumBy)

import Prelude hiding (pi)

type Pos = (Int,Int)
data Node = Passable | NotPassable deriving (Eq, Show)
type Grid = [[Node]]
type Path = [Pos]

type PosGrid = [[Pos]]

shortestPath :: Grid -> Pos -> Pos -> Path
shortestPath g p1 p2 = undefined

-- Auxiliary functions

-- Detects if a position has branching and returns the possibilities.
branch :: PosGrid -> Pos -> [Pos]
branch g p = map snd $ filter ((==1) . abs . fst) $ map (split (uncurry (+) . prod abs abs . subPair p) id) $ concat g

split f g x = (f x, g x)

prod f g (x,y) = (f x, g y)

subPair (a,b) (c,d) = (a-c, b-d)

-- The main algorithm

-- Some types to make stuff easier.
type InitGridEnd = ((Pos, PosGrid), Pos)
type Solution = [[Pos]]
type Acc = [Pos]
type Past = [Pos] -- positions that we already passed by

-- Ramify.
phase1 :: InitGridEnd -> Acc -> Past -> Solution
phase1 ((pi,pg),pe) acc h   | pi == pe = [reverse $ pi : acc]
                            | length (branch pg pi) > 1 = foldr (++) [] $ map (\p -> phase1 ((p,pg),pe) (pi:acc) (pi:h)) (branch pg pi \\ h)
                            | otherwise = phase1 ((head $ branch pg pi, pg),pe) (pi:acc) (pi:h)

-- Check for minimum.
phase2 :: Solution -> Path
phase2 = minimumBy (\x y -> compare (length x) (length y))

-- Testing
pg1 :: PosGrid; pg1 = posGridGen (1,1)
pg4 :: PosGrid; pg4 = posGridGen (100,100)
pi :: (Int,Int); pi = (0,0)
pe :: (Int, Int); pe = (1,1)

posGridGen (x,y) = reverse $ posGridGenAux (0,0) (x,y) [] where
    posGridGenAux (a,b) (x,y) l | a /= x && b /= y = posGridGenAux (a+1, 0) (x,y) (map ((,) a) [0..y] : l)
                                | otherwise = map ((,) a) [0..y] : l