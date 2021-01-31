module TreeGrowth where

import Probability
import Visualize


type Height = Int

data Tree = Alive Height | Hit Height | Fallen
	    deriving (Ord,Eq,Show)

grow :: Trans Tree
grow (Alive h) = normal [Alive k | k <- [h+1..h+5]]

hit :: Trans Tree
hit (Alive h) = certainly (Hit h)

fall :: Trans Tree
fall _ = certainly Fallen

evolve :: Trans Tree
evolve t@(Alive _) = unfoldT (enumPC [90,4,6] [grow,hit,fall]) t
evolve t           = certainly t
-- evolve t@(Alive _) = unfoldT (enum [0.9,0.04,0.06] [grow,hit,fall]) t

-- tree growth simulation:
--   start with seed and run for n generations
-- 
seed :: Tree
seed = Alive 0


-- exact results
--  tree n : tree distribution after n generations
--  hist n : history of tree distributions for n generations
--  
tree n = n *. evolve
hist n = n *.. evolve


-- simulation results
-- 
-- Since *. is overloaded for Trans and RChange, we can run the 
-- simulation ~. directly to 'n *. live'.
-- 
--simTree k n = k ~. tree n
simTree k n = (k,n) ~*. evolve
simHist k n = (k,n) ~.. evolve

t2  = tree 2 seed
h2  = hist 2 seed
st2 = printR $ simTree 2000 2 seed
sh2 = printR $ simHist 2000 2 seed


-- Alternatives:
-- 
-- simTree k n = k ~. n *. random evolve
-- simTree k n = (k,n) ~*. evolve


-- take a trace


height :: Tree -> Int
height Fallen = 0
height (Hit h) = h
height (Alive h) = h
{--
myPlot = plotD ((5 *. evolve) (Alive 0) >>= height)

myPlot2 = figP figure{title="Tree Growth",xLabel="Height (m)",
                yLabel="Probability"}   
                (autoColor [
		plotD ((5 *. evolve) (Alive 0) >>= height)
		])

--}


p1 = fig [plotD $ normal ([1..20]::[Int])]

p2 = fig [plotD $ mapD height (tree 5 seed)]

p3 = figP figure{title="Tree Growth",
            xLabel="Height (ft)",
            yLabel="Probability"}
	    [plotD $ mapD height (tree 5 seed)]


p4 = figP figure{title="Tree Growth",
            xLabel="Height (ft)",
            yLabel="Probability"}
            [heightAtTime 5, heightAtTime 10,heightAtTime 15]

heightAtTime :: Int -> Plot
heightAtTime y = plotD $ mapD height (tree y seed)

p5 = figP figure{title="Tree Growth",
            xLabel="Height (ft)",
            yLabel="Probability"}
            (map heightAtTime [3,5,7])

heightCurve :: (Int,Color) -> Plot
heightCurve (n,c) = (heightAtTime n){color=c,label=show n++" Years"}

p6 = figP figure{title="Tree Growth",   
            xLabel="Height (ft)",
            yLabel="Probability"}
            (map heightCurve 
	    [(3,Blue),(5,Green),(7,Red)])  


done (Alive x) = x >= 5
done _ = True

ev5 = Probability.until done evolve
