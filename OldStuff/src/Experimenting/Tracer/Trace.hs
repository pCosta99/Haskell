{-# LANGUAGE TemplateHaskell #-}

module Trace where

{-
Small experiment to develop a tracer for generic operations.
We'll start with something rather specific, like the function length.
We'll use the state monad so we allow the user to move forward on the computations.
-}

import Control.Monad.State.Lazy
import Cp
import Control.Lens
import List

-- A state contains the computation we want to perform.
data Tracer a b = T {
    _input :: [a],
    _function :: [a] -> b,
    _function_name :: String,
    _hidden_str_acc :: String
}

$(makeLenses ''Tracer)

llength :: [a] -> Int
llength = length

tracer = T [1,2,3] llength "llength" ""

head_extractor = cond ((==0) . length) (split nil nil) (split tail (singl . head))
unit = const ()

go_through :: (Show a, Show b) => StateT (Tracer a b) IO () 
go_through = do
    s <- get
    let (rest,h) = head_extractor $ s ^. input
    let f = s^.function
    let n = s^.function_name
    let str = (show $ f h) ++ " op "
    if ((==0) $ length h) then liftIO $ putStr (s^.hidden_str_acc) >> putStrLn (show $ f h) else liftIO $ putStr (s^.hidden_str_acc) >> putStr str >> putStrLn ("(" ++ n ++ " " ++ show rest ++ ")") 
    put ((hidden_str_acc %~ (str ++)) . (input %~ (const rest)) $ s)
    cond ((==0) . length) (return . unit) (const go_through) h

somatorio :: (Monad m, Num a) => [m a] -> m a
somatorio [] = return 0 
somatorio (h:t) = liftM2 (+) h (somatorio t)