-- 
-- Lotka-Volterra predator-prey model
-- 

module Predator where

import Visualize


-- parameters
--
-- g : victims' growth factor
-- d : predators' death factor
-- s : search rate
-- e : energetic efficiency
-- 

-- try: n>=500
-- g = 1.05
-- d = 0.95
-- s = 0.01
-- e = 0.01

g = 1.02
d = 0.98
s = 0.01
e = 0.01


-- 'direct' function-over-time approach -- very inefficient due to recursion
-- 
-- v :: Int -> Float
-- v 0 = 20
-- v t = ((1 + r - a*p(t-1)) * v (t-1)) `max` 0
-- 
-- p :: Int -> Float
-- p 0 = 15
-- p t = ((1 - d + a*b*v(t-1)) * p (t-1)) `max` 0
-- 
-- 
-- fig1 = figP figure{title="Predator/Prey Simulation "++
--                          showParams [r,d,a,b] ["r","d","a","b"],
--                    xLabel="Time (generation)",
--                    yLabel="Population"} 
--             [(plotF (0,15,1) v){color=Green,label="Victim"}, 
--              (plotF (0,15,1) p){color=Red,label="Prey"}]

v0 = 1
p0 = 1

dv :: (Float,Float) -> Float
dv (v,p) = (g*v - s*v*p) `max` 0

dp :: (Float,Float) -> Float
dp (v,p) = (d*p + e*v*p) `max` 0

dvp vp = (dv vp,dp vp)

vp = (v0,p0):map dvp vp

vs = map fst vp
ps = map snd vp


fig1 n = figP figure{title="Predator/Prey Simulation "++
                         showParams [g,d,s,e] ["g","d","s","e"],
                   xLabel="Time (generation)",
                   yLabel="Population"} 
            [(plotL (take n vs)){color=Green,label="Victim"}, 
             (plotL (take n ps)){color=Red,label="Prey"}]

