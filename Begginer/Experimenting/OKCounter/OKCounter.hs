module Main where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M

data Status = Go | Stop deriving Eq
type State = (Int,Status)

get_ok :: Int -> IO State
get_ok s = putStrLn ("OK has been said " ++ show (s+1) ++ " times!") >> return ((s+1),Go)

quit_n_save s = writeFile "count.dat" (show s) >> return (s,Stop) 

funcs = M.fromList [("quit",quit_n_save),("",get_ok)]

loop s = do
    str <- getLine 
    (s1,status) <- (fromJust $ M.lookup str funcs) s
    if status == Stop then return s1 else loop s1 

main :: IO ()
main = (fmap read $ readFile "count.dat") >>= loop >> return ()
