module MontyHall where


import Probability hiding (choose)
--import ListUtils (replicate)
import List ( (\\) )
import Monad (liftM)

data Door = A | B | C
            deriving (Eq,Ord,Show)

doors :: [Door]
doors = [A,B,C]

data State = Doors {prize :: Door, chosen :: Door, opened :: Door}
             deriving (Eq,Ord,Show)


-- initial configuration of the game status
--  
start :: State
start = Doors {prize=u,chosen=u,opened=u} where u=undefined


-- Steps of the game:
-- 
-- (1) hide the prize
-- (2) choose a door
-- (3) open a non-open door, not revealing the prize
-- (4) apply strategy: switch or stay
-- 
hide :: Trans State
hide s = uniform [s {prize = d} | d <- doors]

choose :: Trans State
choose s = uniform [s {chosen = d} | d <- doors]

open :: Trans State
open s = uniform [s {opened = d} | d <- doors \\ [prize s,chosen s]]

type Strategy = Trans State

switch :: Strategy
switch s = uniform [s {chosen = d} | d <- doors \\ [chosen s,opened s]]

stay :: Strategy 
stay = idT

game :: Strategy -> Trans State
game s = sequ [hide,choose,open,s]


-- Playing the game
-- 
data Outcome = Win | Lose
               deriving (Eq,Ord,Show)

result :: State -> Outcome
result s = if chosen s==prize s then Win else Lose

eval :: Strategy -> Dist Outcome
eval s = mapD result (game s start)

simEval :: Int -> Strategy -> RDist Outcome
simEval k s = mapD result `fmap` (k ~. game s) start


-- Alternative modeling
--
firstChoice :: Dist Outcome
firstChoice = uniform [Win,Lose,Lose]

switch' :: Trans Outcome
switch' Win  = certainly Lose
switch' Lose = certainly Win
