module Probability where

import Cp (p1, p2, (><))
-- Developed along the reading of the paper Probabilistic Functional Programming in Haskell by Martin Erwig and Steve Kollmansberger

newtype Probability = P ProbRep
type ProbRep = Float

showP :: ProbRep -> String
showP pr = show (pr * 100) ++ " %" 

instance Show Probability where
  show (P p) = showP p

-- 
-- Distributions
-- 
newtype Dist a = D {unD :: [(a,ProbRep)]} deriving Show

instance Functor Dist where
    fmap f (D d) = D [(f x, p) | (x,p) <- d]

instance Applicative Dist where
    pure x = D [(x,1)]
    (D d) <*> (D d') = D [(x y, p*q) | (x,p) <- d, (y,q) <- d'] 

-- bind is the dependent event combination
instance Monad Dist where
  return = pure
  d >>= f  = D [(y,q*p) | (x,p) <- unD d, (y,q) <- unD (f x)]
  fail _   = D []

-- monadic composition of two functions
(>@>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >@> g = (>>= g) . f

-- monadic composition of a list of functions
sequ :: Monad m => [a -> m a] -> a -> m a
sequ = foldl (>@>) return

type Spread a = [a] -> Dist a -- Turns a list of events into a distribution

type Event a = a -> Bool -- predicate

-- sums a unfolded distribution
sumP :: [(a,ProbRep)] -> ProbRep
sumP = sum . map snd

-- Gives us the probability of an event in a certain distribution
(??) :: Event a -> Dist a -> Probability
(??) p = P . sumP . filter (p . fst) . unD

-- joins two distributions in a new one according to a certain function
joinWith :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
joinWith f (D d) (D d') = D [(f x y, p*q) | (x,p) <- d, (y,q) <- d']

-- combines two distibutions
prod :: Dist a -> Dist b -> Dist (a,b)
prod = joinWith (,)

-- impossible distribution
impossible :: Dist a
impossible = fail "" 

-- certain distribution
certainly :: a -> Dist a
certainly = return

-- accumulates the odds of consecutive dice rolls
dice :: Int -> Dist [Int]
dice 0 = certainly []
dice n = joinWith (:) (uniform [1..6]) (dice (n-1))

scale :: [(a,ProbRep)] -> Dist a
scale xs = D (map (\(x,p)->(x,p/q)) xs)
           where q = sumP xs

shape :: (Float -> Float) -> Spread a
shape _ [] = impossible
shape f xs = scale (zip xs ps)
             where incr = 1 / fromIntegral ((length xs) - 1)
                   ps = map f (iterate (+incr) 0)

uniform :: Spread a
uniform = shape (const 1)

-- Selects one value from all the possible, excluding it from the final set
selectOne :: Eq a => [a] -> Dist (a,[a])
selectOne c = uniform [(v, filter (/= v) c) | v <- c]

-- Selects n values from the base set, excluding them from the final set
selectMany :: Eq a => Int -> [a] -> Dist ([a],[a])
selectMany 0 c = return ([],c)
selectMany n c = do (x,c1) <- selectOne c
                    (xs,c2) <- selectMany (n-1) c1
                    return (x:xs,c2)

mapD :: (a -> b) -> Dist a -> Dist b
mapD = fmap

-- repeteadly select elements from a collection
select :: Eq a => Int -> [a] -> Dist [a]
select n = mapD (reverse . p1) . selectMany n