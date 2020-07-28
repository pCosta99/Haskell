import Data.Char
import Data.Bifunctor
import qualified Data.Map as M hiding (split)
import Cp hiding ((!))

data Iso a b = Iso { 
    outI :: (a -> b),
    inI :: (b -> a)
    -- outI . inI == inI . outI
}

-- Some examples of isomorphisms
char_int :: Iso Char Int
char_int = Iso ord chr

forward :: (Char,Int) -> (Char,Int)
forward = (chr >< id) . split (uncurry (+)) p2 . (ord >< id)

back :: (Char,Int) -> (Char,Int)
back = (chr >< id) . split (uncurry (-)) p2 . (ord >< id)

sum_int_char :: Iso (Char,Int) (Char,Int)
sum_int_char = Iso forward back

-- Creates the isomorphism we want for a certain 1-1 mapping
transformers :: (Ord a, Ord b, Functor f) => M.Map a b -> Iso (f a) (f b)
transformers = uncurry Iso . (fmap >< fmap) . ((M.!)><(M.!)) . split id rev_map

rev_map :: (Ord a, Ord b) => M.Map a b -> M.Map b a
rev_map = M.fromList . map swap . M.toList

-- |----------------------------------------------------------------------------------------|
-- |                                      Testing                                           |
-- |----------------------------------------------------------------------------------------|

init_map :: M.Map Char Int
init_map = M.insert 'C' 9 $ M.insert 'B' 8 $ M.insert 'A' 3 $ M.empty where

-- Example of a bidirectional mapping with a sample input
input = map toUpper "bbbbbbbcccaabbc"
rev_input = "CCCCCCCAAABBCCA"