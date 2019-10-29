module MyStateGame where

import Control.Monad.State
import System.Random hiding (split)
import Control.Monad (replicateM)
import Data.List (findIndex)
import Cp (split)
import Data.Maybe (fromJust)

type GameState = [Int] -- player's positions

startState :: Int -> [Int]
startState = flip replicate 0

playGame :: StateT GameState IO String
playGame = do 
    state <- get
    rl <- lift $ replicateM (length state) (randomRIO (6,12))
    let newState = zipWith (+) rl state
    lift $ printRound (zip rl newState) 0
    put newState
    let (b, i) = checkWinner newState
    if b then return ("Player " ++ show (fromJust i) ++ " won!")
         else playGame

checkWinner = split (any c) (findIndex c) where c = (> 100)

printRound :: [(Int,Int)] -> Int -> IO ()
printRound [] _ = return ()
printRound ((x,y):xs) n = do
    putStrLn ("Player " ++ show n ++ " rolled " ++ show x ++ ". He is in " ++ show y ++ "!")
    printRound xs (n+1)

main = putStrLn "Introduce number of players:" >> getLine >>= (\np -> evalStateT playGame (startState (read np))) >>= putStrLn