module Decisions where

import Cp
import Data.Char

{-
Objectives:
-- Develop a generic and flexible tree-like structure that could be used to create a path of decisions

Utilites/Example:
-- Could be very useful as the backend of a game for example.
Think about it for a moment. Every action you make, since the start screen, simply throws into a different state between a set of possible ones.
If you press options you go one place, if you press play you go another, etc..
A decision tree with condition evaluating would allow for a seemless implementation of such a mechanic, with enough flexibility to do whatever you feel like and change implementations easily.

Methods:
Algebra of programs for generic operations on the tree overall and a way to run it maybe
-}


{-
First attempt (27-07-2020):
1. After a day of thinking I can see some ressemblenses of the idea above in rose trees. If the element they hold happens to be a evaluator function that tells you the next tree to choose it seems to maybe work.
Reminder: Rose a = Node a [Rose a]
So, as mentioned, basically i'll try to use a rose tree with a function that will return a index. Definetely doesn't seems perfect but it's a start.

Final observations:
-}

{- 
Algebric definition of a rose tree
-}
data Rose a = Rose a [Rose a]

inRose = uncurry Rose
outRose (Rose x l) = (x,l)

baseRose f g = f >< map g
recRose f = baseRose id f

cataRose g = g . recRose (cataRose g) . outRose
anaRose g = inRose . recRose (anaRose g) . g
hyloRose f g = cataRose f . anaRose g

{-
We'll force the type here because it only make sence to run a rose tree that has functions
-}
runRosePf :: (a,Rose (a -> (a,Int))) -> a
runRosePf = cond ((==0) . length . p2) (p1 . p1) (runRosePf . choose) . transform where
    transform = (ap >< id) . (swap >< id) . assocl . (id >< outRose) 
    choose = (id >< uncurry (flip (!!))) . assocr 

-- Way better tbh
runRose :: a -> Rose (a -> (a,Int)) -> a
runRose state (Rose f l) = if length l == 0 then new else runRose new (l!!index) where 
    (new, index) = f state 

{-
Simple examples to get us started
-}
rose_t1 :: Rose (Char -> (Char,Int))
rose_t1 = base where
    base = Rose f (singl base) -- creates a infinite tree, useless but had to do it xD
    f = (id >< fromInteger) . split (chr . (+1) . ord) (cond (== 'a') zero one)

rose_t2 :: Rose (Char -> (Char,Int))
rose_t2 = Rose f [base [], base []] where
    base g = Rose f g
    f = (id >< fromInteger) . split (chr . (+1) . ord) (cond (== 'a') zero one)

{-
Ok, let's recap, so far we basically did nothing and very unflexibly. Great. Next step, imo, would be adding some new info to our Rose.
Or monadify it. We could use the ability to print some info as we go, should be useful. Let's try this second one first.
-}

{-
Our objective is running the tree we see below basically. If you try to run it with runRose it's gonna blow up cause of IO.
So, what to do... 
-}
rose_t3 :: Rose (Char -> IO (Char, Int))
rose_t3 = Rose g [] where
    g c = putStrLn "sup" >> f c 
    f = return . (id >< fromInteger) . split (chr . (+1) . ord) (cond (== 'a') zero one)

-- Actually easier than what I originally thought, just do it. Let's try pointfree tho
runRoseMonadic :: Monad m => a -> Rose (a -> m (a, Int)) -> m a
runRoseMonadic state (Rose f l) = do
    (new, index) <- f state
    if length l == 0 then return new else runRoseMonadic new (l!!index)

{- 
All that gotta change is pretty much after the ap I guess. Since it's the point where we have recursion and that's what went fucked. 
After done note: This shit was hard.
-}
runRoseMonadicPf :: Strong m => (a,Rose (a -> m (a, Int))) -> m a
runRoseMonadicPf = mult . fmap condition . rstr . transform where
    condition = cond ((==0) . length . p2) (return . p1 . p1) (runRoseMonadicPf . choose)
    transform = (ap >< id) . (swap >< id) . assocl . (id >< outRose) 
    choose = (id >< uncurry (flip (!!))) . assocr

{-
Looking better, a bit more flexible, still, lots of stuff to do. 
The most important one right now is a test. We need to check how we doing in terms of power. Is it enough to do a simple start menu for example?
No clue, but it should be getting there.
Also wanna figure out what I can actually do with those cata's, ana's and hylo's, they should be fun to alter the tree in some way.
-}