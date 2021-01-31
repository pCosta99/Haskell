module RandomPairs where

import System.Random hiding (split)
import Cp
import Data.List
import Control.Monad as M

mygen :: [Int] -> [Int] -> IO Int
mygen l acc = do
	v <- randomRIO (minimum l, maximum l)
	if elem v acc then mygen l acc else return v

mygen_pf :: ([Int],[Int]) -> IO Int
mygen_pf = M.join . (cond (uncurry elem . (id >< p2)) (mygen_pf . p2) (return . p1) <$.> rstr . split (randomRIO . split minimum maximum . p1) id)

infix 5 <$.>

f <$.> g = fmap f . g

genAll :: [Int] -> [Int] -> IO [(Int, Int)]
genAll [] acc = return []
genAll l acc = do
	v1 <- mygen l acc 
	v2 <- mygen (delete v1 l) (v1:acc)
	rest <- genAll (delete v1 $ delete v2 l) (v1:v2:acc)
	return $ ((v1,v2) : rest)

main = genAll [1..34] []