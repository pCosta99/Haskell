module NBoys where


import Probability
import ListUtils

--
-- Ceneralization of Boys.hs
-- 
-- Consider a family of n children.  Given that there are k boys in the family,
-- what is the probability that there are m boys in the family?
-- 

data Child = Boy | Girl
             deriving (Eq,Ord,Show)

type Family = [Child]

birth :: Trans Family
birth f = uniform [Boy:f,Girl:f]

family :: Int -> Dist Family
family n = sequ (replicate n birth) []

countBoys :: Family -> Int
countBoys = length . filter (==Boy)

boys :: Int -> Event Family
boys k f = countBoys f >= k

nBoys :: Int -> Int -> Int -> Probability
nBoys n k m =  (boys m) ?? (family n ||| boys k)

numBoys :: Int -> Int -> Dist Int
numBoys n k = mapD countBoys (family n ||| boys k)


-- 
-- Special cases
-- 

-- only boys in a family that has one boy
-- 
onlyBoys1 n = nBoys n 1 n

