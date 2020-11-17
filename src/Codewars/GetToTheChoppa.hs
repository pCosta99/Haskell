module Kata.GetToTheChoppa where

type Pos = (Int,Int)
data Node = Passable | NotPassable deriving (Eq, Show)
type Grid = [[Node]]
type Path = [Pos]

type PosGrid = [[Pos]]

shortestPath :: Grid -> Pos -> Pos -> Path
shortestPath g p1 p2 = undefined

-- Detects if a position has branching and returns the possibilities.
branch :: PosGrid -> Pos -> [Pos]
branch g p = map snd $ filter ((==1) . fst) $ map (split (uncurry (+) . subPair p) id) $ concat g

split f g x = (f x, g x)

subPair (a,b) (c,d) = (a-c, b-d)

-- Testing
