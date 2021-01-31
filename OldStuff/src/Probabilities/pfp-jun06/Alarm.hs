module Alarm where

import Probability

type PBool = Dist Bool


flp :: Float -> PBool
flp p = choose p True False


-- alarm network

-- prior burglary 1%
b = flp 0.01

-- prior earthquake 0.1%
e = flp 0.001

-- conditional probability of alarm given burglary and earthquake
a b e = case (b,e) of
	(False,False) -> flp 0.01
        (False,True)  -> flp 0.1 
        (True,False)  -> flp 0.7
        (True,True)   -> flp 0.8


-- conditional probability of john calling given alarm
j a = if a then flp 0.8 else flp 0.05

-- conditional probability of mary calling given alarm
m a = if a then flp 0.9 else flp 0.1

-- calculate the full joint distribution
data Burglary = B { 	burglary :: Bool,
			earthquake :: Bool,
			alarm :: Bool,
			john :: Bool,
			mary :: Bool }
	deriving (Eq, Ord, Show)		
	
bJoint = do b' <- b 		-- burglary
            e' <- e 		-- earthquake
            a' <- a b' e' 	-- alarm
	    j' <- j a' 		-- john
	    m' <- m a' 		-- mary
	    return (B b' e' a' j' m')

-- what is the probability that mary calls given that john calls?
pmj = mary ?? bJoint ||| john

	      
	
