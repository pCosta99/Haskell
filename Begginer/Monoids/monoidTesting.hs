-- Most of the code developed in this module was created accordingly to the article "Haskell Monoids and their Uses" from sigfpe blog.
-- Link --> http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html
module MonoidTesting where

import Data.Monoid
import Control.Monad.Writer
import Data.Foldable
import Cp

-- Monoids to look for minimum and maximum values
newtype Max a = Max {getMax :: a} deriving (Eq, Ord, Show)
instance Num a => Num (Max a) where
        (+) = (+)
        (*) = (*)
        abs = abs
        signum = signum
        fromInteger = fromInteger
        negate = negate

instance Ord a => Semigroup (Max a) where
        a <> b = if a > b then a else b  

instance (Ord a, Num a) => Monoid (Max a)  where
        mempty = Max (-9999999) -- negative infinity

newtype Min a = Min {getMin :: a} deriving (Eq, Ord, Show)
instance Num a => Num (Min a) where
        (+) = (+)
        (*) = (*)
        abs = abs
        signum = signum
        fromInteger = fromInteger
        negate = negate

instance Ord a => Semigroup (Min a) where
        a <> b = if a > b then b else a

instance (Ord a, Num a) => Monoid (Min a)  where
        mempty = Min 9999999 -- infinity

-- Logging functions with the use of Writer Monad and Monoid folding
-- The function we will be logging will be zip

-- Pointfree awesome example
myZip :: [a] -> [a] -> [(a,a)]
myZip = foldr f nil where
        f _ _ [] = []
        f x r (y:ys) = (x,y) : r ys

-- Logging number of zipped pairs
myZip_LogZips :: [a] -> [a] -> Writer (Sum Integer) [(a,a)]
myZip_LogZips _ [] = return []
myZip_LogZips [] _ = return []
myZip_LogZips (x:xs) (y:ys) = do
        let p = (x,y)
        tell $ Sum 1
        l <- myZip_LogZips xs ys
        return (p : l)

-- Logging every pair as a string
myZip_LogZips2 :: (Show a) => [a] -> [a] -> Writer [String] [(a,a)]
myZip_LogZips2 _ [] = return []
myZip_LogZips2 [] _ = return []
myZip_LogZips2 (x:xs) (y:ys) = do
        let p = (x,y)
        tell $ singl $ "Created pair " ++ show p ++ "!"
        l <- myZip_LogZips2 xs ys
        return (p : l)

-- Logging every pair as a string in the reversed order
myZip_LogZipsDual :: (Show a) => [a] -> [a] -> Writer (Dual [String]) [(a,a)]
myZip_LogZipsDual _ [] = return []
myZip_LogZipsDual [] _ = return []
myZip_LogZipsDual (x:xs) (y:ys) = do
        let p = (x,y)
        tell $ Dual $ singl $ "Created pair " ++ show p ++ "!"
        l <- myZip_LogZipsDual xs ys
        return (p : l)

-- Logging number of zipped pairs and info about them
myZip_LogZips3 :: (Show a) => [a] -> [a] -> Writer (Sum Integer, [String]) [(a,a)]
myZip_LogZips3 _ [] = return []
myZip_LogZips3 [] _ = return []
myZip_LogZips3 (x:xs) (y:ys) = do
        let p = (x,y)
        tell $ (Sum 1, singl $ "Created pair " ++ show p ++ "!")
        l <- myZip_LogZips3 xs ys
        return (p : l)

-- BTree a
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Foldable Tree where
        foldMap f Empty = mempty
        foldMap f (Leaf x) = f x
        foldMap f (Node l k r) = foldMap f l <> f k <> foldMap f r

tree = Node (Leaf 1) 7 (Node Empty 3 (Leaf 2))
list = [7,2,8,4,5]
pair = (4,9)

-- Querying the tree so easily
-- Sum
sumF :: (Foldable t, Num a) => t a -> Sum a
sumF = foldMap Sum

-- Product
productF :: (Foldable t, Num a) => t a -> Product a
productF = foldMap Product

-- to list
toListF :: (Foldable t) => t a -> [a]
toListF = foldMap singl

-- max
maxF :: (Num a, Ord a, Foldable t) => t a -> Max a
maxF = foldMap Max

-- min
minF :: (Num a, Ord a, Foldable t) => t a -> Min a
minF = foldMap Min

-- both max and min in one go
maxminF :: (Num a, Ord a, Foldable t) => t a -> (Min a, Max a)
maxminF = foldMap $ split Min Max

-- any match
anyF :: (Foldable t) => (a -> Bool) -> t a -> Any
anyF p = foldMap $ Any . p

-- all match
allF :: (Foldable t) => (a -> Bool) -> t a -> All
allF p = foldMap $ All . p