module Utilities where

infix 1 </>
infix 1 >>*

x </> y = x ++ "/" ++ y 

(f >>* g) x = (f x) >> (g x)