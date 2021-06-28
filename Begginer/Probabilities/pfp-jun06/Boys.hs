module Boys where


import Probability
import ListUtils

--
-- Consider a family of two children.  Given that there is a boy in the family,
-- what is the probability that there are two boys in the family?
-- 

data Child = Boy | Girl
             deriving (Eq,Ord,Show)

type Family = [Child]

birth :: Trans Family
birth f = uniform [Boy:f,Girl:f]

family :: Dist Family
family = sequ [birth,birth] []

-- NOTE: could be fixed to 2
--       could be renamed to allBoys
-- 
boys :: Int -> Event Family
boys n = just (replicate n Boy)

existsBoy :: Event Family
existsBoy = elem Boy

-- NOTE: might not be needed, i.e., definition can be inlined instead
-- 
familyWithBoy :: Dist Family
familyWithBoy = family ||| existsBoy

twoBoys :: Probability
twoBoys = (boys 2) ?? familyWithBoy


countBoys :: Family -> Int
countBoys = length . filter (==Boy)

numBoys :: Dist Int
numBoys = mapD countBoys familyWithBoy

