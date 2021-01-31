module Fact where

import List
import Nat
import Cp
import Data.List.Ordered

unit = const ()

-- Gets all primes that exist ordered from low to higher
primes = map head $ scanl (\\) [2..] [[p, p+p..] | p <- primes]
                       where 
                          (\\) = Data.List.Ordered.minus

findLeastPrime :: Int -> Int
findLeastPrime i = head $ dropWhile ((/=0) . mod i) primes

fact = anaList $ cond (==1) (i1 . unit) (i2 . g) where
    g x = let n = findLeastPrime x in (n, div x n)

pf_fact = anaList $ cond (==1) (i1 . unit) (i2 . k) where
    k = split p2 (uncurry div) . split id findLeastPrime
    findLeastPrime' i = head $ dropWhile ((/=0) . mod i) primes
    primes = map head $ scanl minus [2..] [[p, p+p..] | p <- primes]