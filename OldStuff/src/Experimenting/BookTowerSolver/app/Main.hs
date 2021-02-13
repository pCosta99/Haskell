module Main where

import qualified Data.Vector as V
import Control.Monad.State.Lazy
import Data.Maybe
import Control.Arrow
import Data.List

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

type Towers = V.Vector Column

-- P : Purple
-- LB : Light Blue
-- O : Orange
-- R : Red
-- B : Blue
-- G : Green
-- Towers are made bottom to top in each column!
trs :: Towers
trs = V.fromList [c0,c1,c2,c3,c4,c5,c6,c7] where
    c0 = V.fromList [B "B" 3, B "R" 6, B "O" 3, B "B" 6, B "LB" 2, B "P" 5]
    c1 = V.fromList [B "O" 5, B "G" 0, B "O" 0, B "LB" 6, B "R" 0, B "LB" 4]
    c2 = V.fromList [B "O" 4, B "R" 3, B "O" 1, B "R" 2, B "B" 4]
    c3 = V.fromList [B "B" 5, B "B" 0, B "P" 3, B "P" 0, B "G" 1]
    c4 = V.fromList [B "G" 3, B "LB" 1, B "P" 1, B "LB" 3, B "P" 6]
    c5 = V.fromList [B "O" 6, B "LB" 0, B "B" 1, B "P" 2, B "G" 6]
    c6 = V.fromList [B "LB" 5, B "B" 2, B "R" 4, B "R" 5, B "R" 1]
    c7 = V.fromList [B "G" 4, B "O" 2, B "P" 4, B "G" 2, B "G" 5]

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
perform (MI (s,d) seq _ sc) t = t V.// [(s,new_source),(d,new_destiny)] where
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
buildMoveInfo m@(s,d) t = MI m seq dest_col source_col where
    seq = getSequence $ t V.! s
    dest_col = t V.! d
    source_col = t V.! s

---------------------- SOLVER --------------------

-- Obtains all valid moves for a certain state
validMoves :: Towers -> [Move]
validMoves t = filter (legal . flip buildMoveInfo t) all where
    is = [0 .. V.length t - 1]
    all = [(x,y) | x <- is, y <- is, x /= y]

---------------------- EXTRAS --------------------
validTowers :: Towers -> Bool
validTowers = check . V.concat . V.toList where
    check t = all (`elem` t) ([B x y | x <- colors t, y <- [0..6]])
    no_reps = uncurry (==) . (nubBy (=.=) &&& id)
    colors = nub . V.toList . V.map color

doMove :: Move -> State Towers String
doMove move = do
    towers <- get
    let result = makeMove move towers
    if isNothing result then return "Rip" else put (fromJust result) >> return "Sucess"

getValidMoves :: State Towers [Move]
getValidMoves = validMoves <$> get

main = undefined
