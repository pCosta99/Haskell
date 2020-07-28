module Navigator where

import Tablify as T
import Cp
import List
import Data.Maybe
import Data.List.Split hiding (split)
import System.Console.Terminal.Size

data Navigator = Nav {
    pages :: [T.TableInfo],
    n_pages :: Int
}

init_nav :: (Show a) => [a] -> [[a]]
init_nav l = chunksOf splitIn l where
    p = fmap (split height width . fromJust) size
    avg_length = (sum $ map (length . show) l) `div` (length l)
    splitIn = (length l) `div` avg_length

cata :: Int -> [String] -> [[String]]
cata lim = p1 . (cataList $ either (split nil (const 0)) g) where
    g = Cp.cond ((lim <) . acc_size) h f
    h = split (cons . (cons >< id) . assocl . (id >< (split head tail . p1))) acc_size
    f = split (cons . (singl >< p1)) acc_size
    acc_size = uncurry (+) . (length >< p2)