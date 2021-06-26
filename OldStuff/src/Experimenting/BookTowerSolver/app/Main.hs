module Main where

import qualified Data.Vector as V
import Control.Monad.State.Lazy
import Data.Maybe
import Control.Arrow
import Data.List

import Data.Graph.Inductive.Graph hiding (match)
import Data.Graph.Inductive.PatriciaTree(Gr)
import Data.Graph.Inductive.Query.SP

type Color = String

data Book = B {
    color :: Color,
    number :: Int
} deriving Show

instance Eq Book where
    (B c1 _) == (B c2 _) = c1 == c2

(=.=) :: Book -> Book -> Bool
(B c1 t1) =.= (B c2 t2) = c1 == c2 && t1 == t2

instance Ord Book where
    (B _ t1) <= (B _ t2) = t1 <= t2

type Column = V.Vector Book
type Sequence = V.Vector Book -- Sequence of books, they maintain a correct order

data Towers = T (V.Vector Column)

instance Show Towers where
    show _ = "T"

-- P : Purple
-- LB : Light Blue
-- O : Orange
-- R : Red
-- B : Blue
-- G : Green
-- Towers are made bottom to top in each column!
trs :: Towers
trs = T $ V.fromList [c0,c1,c2,c3,c4,c5,c6,c7] where
    c0 = V.fromList [B "B" 3, B "R" 6, B "O" 3, B "B" 6, B "LB" 2, B "P" 5]
    c1 = V.fromList [B "O" 5, B "G" 0, B "O" 0, B "LB" 6, B "R" 0, B "LB" 4]
    c2 = V.fromList [B "O" 4, B "R" 3, B "O" 1, B "R" 2, B "B" 4]
    c3 = V.fromList [B "B" 5, B "B" 0, B "P" 3, B "P" 0, B "G" 1]
    c4 = V.fromList [B "G" 3, B "LB" 1, B "P" 1, B "LB" 3, B "P" 6]
    c5 = V.fromList [B "O" 6, B "LB" 0, B "B" 1, B "P" 2, B "G" 6]
    c6 = V.fromList [B "LB" 5, B "B" 2, B "R" 4, B "R" 5, B "R" 1]
    c7 = V.fromList [B "G" 4, B "O" 2, B "P" 4, B "G" 2, B "G" 5]

trsSolved :: Towers
trsSolved = T $ V.fromList [c0,c1,c2,c3,c4,c5,c6,c7] where
    c0 = V.fromList [B "B" 6, B "B" 5, B "B" 4, B "B" 3, B "B" 2]
    c1 = V.fromList [B "O" 6, B "O" 5, B "O" 4, B "O" 3, B "O" 2, B "O" 1, B "O" 0]
    c2 = V.fromList [B "R" 6, B "R" 5, B "R" 4, B "R" 3, B "R" 2, B "R" 1, B "R" 0]
    c3 = V.fromList [B "LB" 6, B "LB" 5, B "LB" 4, B "LB" 3, B "LB" 2, B "LB" 1, B "LB" 0]
    c4 = V.fromList [B "P" 6, B "P" 5, B "P" 4, B "P" 3, B "P" 2, B "P" 1, B "P" 0]
    c5 = V.fromList [B "G" 6, B "G" 5, B "G" 4, B "G" 3, B "G" 2, B "G" 1, B "G" 0]
    c6 = V.fromList [B "B" 0]
    c7 = V.fromList [B "B" 1]
---------------------- MOVING --------------------

-- From which column to which column
type Move = (Int,Int)

data MoveInfo = MI {
    move :: Move,
    seq :: Sequence,
    dest_col :: Column,
    source_col :: Column
}

-- Checks if a movement is legal or not
legal :: MoveInfo -> Bool
legal (MI _ seq dc _) = match (V.head seq) (V.last dc)

-- Makes the move if it is legal
makeMove :: Move -> Towers -> Maybe Towers
makeMove m@(s,d) t = if legal move_info then Just $ perform move_info t else Nothing where
    move_info = buildMoveInfo m t

-- Performs the movement
perform :: MoveInfo -> Towers -> Towers
perform (MI (s,d) seq _ sc) (T t) = T $ t V.// [(s,new_source),(d,new_destiny)] where
    new_source = V.slice 0 (V.length sc - V.length seq) sc
    new_destiny = (t V.! d) V.++ seq

-- Extracts the sequence that will be moved
getSequence :: Column -> Sequence
getSequence c = if l >= 2 && cond (V.last c) (c V.! (l-2)) then getSequence (V.init c) V.++ boxed_last else boxed_last where
    l = V.length c
    cond b1@(B _ t1) b2@(B _ t2) = match b1 b2 && t1 == (t2 - 1)
    boxed_last = V.slice (l-1) 1 c

-- True if they can be stacked
match :: Book -> Book -> Bool
match b1 b2 = b1 == b2 && b1 <= b2

buildMoveInfo :: Move -> Towers -> MoveInfo
buildMoveInfo m@(s,d) (T t) = MI m seq dest_col source_col where
    seq = getSequence $ t V.! s
    dest_col = t V.! d
    source_col = t V.! s

---------------------- SOLVER --------------------

-- Obtains all valid moves for a certain state
validMoves :: Towers -> [Move]
validMoves ts@(T t) = filter (legal . flip buildMoveInfo ts) all where
    is = [0 .. V.length t - 1]
    all = [(x,y) | x <- is, y <- is, x /= y]

-- Given a setup finds the solution to solve it
--solve :: Towers -> [[Move]]
--solve t = solveAux t []

--solveAux :: Towers -> [Move] -> [[Move]]
--solveAux t acc = do
    --let movesDone = map (id &&& (fromJust . flip makeMove t)) $ validMoves t
    --if any (isSolved . snd) movesDone
        --then (:[]) $ acc ++ [fst $ fromJust (find (isSolved . snd) movesDone)]
        --else concatMap (flip solveAux acc) $ map snd movesDone

-- Indicates if a setup is finished
isSolved :: Towers -> Bool
isSolved (T t) = all (\col -> sameColor col && ordered col && allOrNone col) $ V.map V.toList t where
    sameColor [] = True
    sameColor (h:t) = all (==h) t
    ordered l = and $ zipWith (>=) l (tail l)
    allOrNone l = length l == 7 || length l == 0

---------------------- Into Graphs --------------------
-- From initial towers, create all possible paths and put them in a graph.
-- Keep in memory what has already been created to avoid getting stuck in cycles.
-- The last element provided stores which nodes where solutions to the problem.
buildGraph :: Towers -> ([LNode (Towers,[Move])], [LEdge Int], [Int])
buildGraph t = snd $ buildGraphAux t 0 ([],[],[])

buildGraphAux :: Towers
              -> Int -- Index of the node we are about to build
              -> ([LNode (Towers, [Move])], [LEdge Int], [Int])
              -> (Int,([LNode (Towers,[Move])], [LEdge Int], [Int]))
buildGraphAux t i acc@(ln, le, s) = if isSolved t then (i,(ln, le, i:s)) else rec i movesDone where
    movesDone = map (id &&& (fromJust . flip makeMove t)) $ validMoves t
    rec n []  = (n,([],[],[]))
    rec n (md@(m,t):ms) = let (n',sol) = bga n m t in merge (n', sol) (rec n' ms)
    bga n m t = buildGraphAux t (n+1) ([(n+1,(t,[m]))], [(n,n+1,1)], [])
    merge (n,(ln,le,s)) (n',(ln',le',s')) = (max n n', (ln ++ ln', le ++ le', s ++ s'))

main = undefined
