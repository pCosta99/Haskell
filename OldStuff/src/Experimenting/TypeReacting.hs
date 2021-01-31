module TypeReacting where

import Cp
import Data.Char

elemAll :: (Eq a, Foldable t) => Either (a, t a) (Char, String) -> Bool
elemAll = either (uncurry elem) (uncurry elem . (id >< map toLower))

elemAll1 :: (Eq b) => (a -> b) -> a -> [a] -> Bool
elemAll1 _ a [] = False
elemAll1 f a (h:t) | f a == f h = True
                   | otherwise = elemAll1 f a t
