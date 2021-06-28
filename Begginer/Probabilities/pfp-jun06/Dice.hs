module Dice where

import Probability


type Die = Int

die :: Dist Die
die = uniform [1..6]

twoDice :: Dist (Die,Die)
twoDice = prod die die

dice :: Int -> Dist [Die]
dice 0 = certainly []
dice n = joinWith (:) die (dice (n-1))


twoSixes = (==[6,6]) ?? dice 2

-- sixes p n computes the probability of getting
-- p sixes (>1,==2,...) when rolling n dice
--
sixes :: (Int -> Bool) -> Int -> Probability
sixes p n = (p . length . filter (==6)) ?? dice n

plus1 x = choose 0.5 x (x+1)

droll = do
	d <- die
	plus1 d

g3 = (>3) ?? die

addTwo = do
        d1 <- die
        d2 <- die
        return (d1+d2)

