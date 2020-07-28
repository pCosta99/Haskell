module Probability where

import Cp (p1, p2, (><))
import Show (showR)
import Data.List (sort)
import System.Random (randomRIO)
-- Developed along the reading of the paper Probabilistic Functional Programming in Haskell by Martin Erwig and Steve Kollmansberger

newtype Probability = P ProbRep
type ProbRep = Float

precision :: Int
precision = 1

showPfix :: ProbRep -> String
showPfix f | precision==0 = showR 3 (round (f*100))++"%"
           | otherwise    = showR (4+precision) (fromIntegral (round (f*100*d))/d)++"%"
             where d = 10^precision

-- fixed precision
--
showP :: ProbRep -> String
showP = showPfix

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
  fail _ = D []
{-
instance MonadFail Dist where
    fail _ = D []
-}
-- monadic composition of two functions
(>@>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >@> g = (>>= g) . f

-- monadic composition of a list of functions
sequ :: Monad m => [a -> m a] -> a -> m a
sequ = foldl (>@>) return

type Spread a = [a] -> Dist a -- Turns a list of events into a distribution

type Event a = a -> Bool -- predicate

type Trans a = a -> Dist a

-- sums a unfolded distribution
sumP :: [(a,ProbRep)] -> ProbRep
sumP = sum . map snd

-- Gives us the probability of an event in a certain distribution
(??) :: Event a -> Dist a -> Probability
(??) p = P . sumP . filter (p . fst) . unD

-- joins two distributions in a new one according to a certain function
joinWith :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
joinWith f (D d) (D d') = D [(f x y, p*q) | (x,p) <- d, (y,q) <- d']

-- combines two distributions
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

-- Calculating distributions ------------------------------------------------
-- auxiliary functions for constructing and working with distributions
onD :: ([(a,ProbRep)] -> [(a,ProbRep)]) -> Dist a -> Dist a
onD f  = D . f . unD

-- normalization = grouping
--
normBy ::  Ord a => (a -> a -> Bool) ->  Dist a -> Dist a
normBy f = onD $ accumBy f . sort

accumBy :: Num b => (a -> a -> Bool) -> [(a,b)] -> [(a,b)]
accumBy f ((x,p):ys@((y,q):xs)) | f x y     = accumBy f ((x,p+q):xs)
                                | otherwise = (x,p):accumBy f ys
accumBy _ xs = xs

norm ::  Ord a => Dist a -> Dist a
norm = normBy (==)
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
-- -------------------------------------------------------------------------

-- Selects one value from all the possible, excluding it from the final set
selectOne :: Eq a => [a] -> Dist (a,[a])
selectOne c = uniform [(v, filter (/= v) c) | v <- c]

-- Selects one value from all the possible, without excluding it from the final set
selectOneAndKeep :: Eq a => [a] -> Dist (a,[a])
selectOneAndKeep c = uniform [(v, c) | v <- c]

-- Selects n values from the base set, excluding them from the final set
selectMany :: Eq a => Int -> [a] -> Dist ([a],[a])
selectMany 0 c = return ([],c)
selectMany n c = do (x,c1) <- selectOne c
                    (xs,c2) <- selectMany (n-1) c1
                    return (x:xs,c2)

-- Selects n values from the base set, without excluding them from the final set
selectManyAndKeep :: Eq a => Int -> [a] -> Dist ([a],[a])
selectManyAndKeep 0 c = return ([],c)
selectManyAndKeep n c = do (x,c1) <- selectOneAndKeep c
                           (xs,c2) <- selectManyAndKeep (n-1) c1
                           return (x:xs,c2)

mapD :: (a -> b) -> Dist a -> Dist b
mapD = fmap

-- repeteadly select elements from a collection
select :: Eq a => Int -> Trans [a]
select n = mapD (reverse . p1) . selectMany n

-- repeteadly select elements from a collection without removing them
selectWhileKeeping :: Eq a => Int -> Trans [a]
selectWhileKeeping n = mapD (reverse . p1) . selectManyAndKeep n

-- Randomization
type R a = IO a
type RChange a = a -> R a

type RDist a = R (Dist a)
type RTrans a = a -> RDist a

-- picks a random value from a distribution
pick :: Dist a -> R a
pick d = randomRIO (0,1) >>= (return . selectP d)

random :: Trans a -> RChange a
random t = pick . t

-- selecting from distributions
--
selectP :: Dist a -> ProbRep -> a
selectP (D d) p = scanP p d

scanP :: ProbRep -> [(a,ProbRep)] -> a
scanP p ((x,q):ps) | p<=q || null ps = x
                   | otherwise       = scanP (p-q) ps

rDist :: Ord a => [R a] -> RDist a
rDist = fmap (norm . uniform) . sequence
