module Queuing where


import Probability
import List (nub,sort)
import ListUtils

{-

Model:

  one server serving customers from one queue

-}

type Time = Int

type Profile = (Time, Time) -- (servingTime, nextArrival)

type Event a = (a,Profile)

type Queue a = [(a,Time)]      -- customers and their individual serving times

type State a = (Queue a,Time)  -- (customers waiting,validity period of that queue)

type System a = [([a],Time)]

type Events a = [Queuing.Event a]


event :: Time -> Events a -> Queue a -> [State a]
event = mEvent 1

--event _ [] []                    = []
--event 0 ((c,(s,a)):es) q         =        event a     es (q++[(c,s)])
--event a es []                    = ([],a):event 0     es []
--event a [] (q@((c,s):q'))        =  (q,s):event a     [] q' 
--event a es (q@((c,s):q')) | a<s  =  (q,a):event 0     es ((c,s-a):q')
--                          | True =  (q,s):event (a-s) es q'

system :: Events a -> System a
--system es = map (\(q,t)->(map fst q,t)) $ event 0 es []
system = mSystem 1


-- multiple servers

mEvent :: Int -> Time -> Events a -> Queue a -> [State a]
mEvent _ _ [] []             =        []
mEvent n 0 ((c,(s,a)):es) q  = 	      mEvent n a     es (q++[(c,s)])
mEvent n a es []             = ([],a):mEvent n 0     es []
mEvent n a [] q		     =  (q,s):mEvent n 0     [] (mServe n s q)
	where s = mTimeStep n q
mEvent n a es q | a < s      =  (q,a):mEvent n 0     es (mServe n a q)
		| True	     =  (q,s):mEvent n (a-s) es (mServe n s q)
	where s = mTimeStep n q


-- decrease served customers remaining time by specified amount
mServe :: Int -> Int -> Queue a -> Queue a
mServe _ _ [] = []
mServe 0 _ x = x
mServe n c ((a,t):es) | t > c = (a,t-c):(mServe (n-1) c es)
		    | True  = mServe (n-1) c es

-- time until next completion
mTimeStep :: Int -> Queue a -> Int
mTimeStep _ ((_,t):[]) = t
mTimeStep 1 ((_,t):es) = t
mTimeStep n ((_,t):es) = min t (mTimeStep (n-1) es)

mSystem :: Int -> Events a -> System a
mSystem n es = map (\(q,t)->(map fst q,t)) $ mEvent n 0 es []

remove 0 lx = lx
remove _ [] = []
remove n (l:lx) = remove (n-1) lx


-- random

type RProfile = (Dist Time, Trans Time)

type REvent a = (a, RProfile)

type REvents a = [REvent a]

rSystem :: Int -> REvents a -> R (System a)
rSystem n re = do
		e <- rBuildEvents re
		return (mSystem n e)

rBuildEvents :: REvents a -> R (Events a)
rBuildEvents ((a,(dt,tt)):ex) = do
			rest <- rBuildEvents ex
			t <- pick dt
			nt <- pick $ tt t
			return ((a,(t,nt)):rest)
rBuildEvents [] = return []

rmSystem :: Ord a => Int -> Int -> REvents a -> RDist (System a)
rmSystem c n re = rDist $ replicate c (rSystem n re)

evalSystem :: Ord a => Int -> Int -> REvents a -> (System a -> b) -> RDist b
evalSystem c n re ef = do
			rds <- rmSystem c n re
			return (mapD ef rds)

unit = (\p->((),p)) -- mapD (\p->((),p))			


-- evaluation

maxQueue :: Ord a => System a -> Int
maxQueue s = maximum [length q | (q,_) <- s]

allWaiting :: Ord a => Int -> System a -> [a]
allWaiting n s = nub $ sort $ concat [ remove n q | (q,_) <- s]


countWaiting :: Ord a => Int -> System a -> Int
countWaiting n = length . (allWaiting n)

waiting :: Int -> System a -> Time
waiting n s = sum [ t*length q' | (q,t) <- s, let q' = remove n q] 

inSystem :: System a -> Time
inSystem s = sum [ t*length q | (q,t) <- s]

total :: System a -> Time
total = sum . map snd

server :: Int -> System a -> Time
server n s = sum [ t*length q' | (q,t) <- s, let q' = take n q] 

idle :: Int -> System a -> Time 
idle n s = sum [ t*(n - length q) | (q,t) <- s, length q <= n] 

idleAvgP :: Int -> System a -> Float
idleAvgP n s = (fromIntegral $ idle n s) / (fromIntegral $ server n s)


