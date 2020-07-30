module Decisions where

import Cp
import Data.Char
import Control.Monad.State.Lazy hiding (ap)

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
-}

{-
Second day (30-07-2020)
Won't deploy much time today, will do a very simple example to check how we doing right now, a simple menu with some options
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
runRoseMonadic :: Monad m => a -> Rose (a -> m (Int, a)) -> m a
runRoseMonadic state (Rose f l) = do
    (index, new) <- f state
    if length l == 0 then return new else runRoseMonadic new (l!!(index-1))

{- 
All that gotta change is pretty much after the ap I guess. Since it's the point where we have recursion and that's what went fucked. 
After done note: This shit was hard.
-}
-- need to change to (a -> m (Int,a))
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

{-
Aight, so, beggining of day 2. I don't really need to pass input here since it's the actual functions who will be asking stuff... 
We'll pass void, aka (), just because we need to. Ideally it should be some sort of state, so, actually, let's define a void state first.
-}

data TState = S Int

init_state = S 0

-- Ok, so, i was writing down this signature and just noticed that this looks a lot like State monad... Might need to check how it comes together. Day 3 goal.
simple_menu :: Rose (TState -> IO (Int, TState))
simple_menu = Rose start [Rose options [], Rose quit [], Rose cheer_up []] where
    start s = putStrLn "1. Options" >> putStrLn "2. Quit" >> putStrLn "3. Cheer up!" >> getCharAndReact s
    options s = putStrLn "1. Augment volume" >> putStrLn "2. Decrease volume" >> getCharAndReact s
    quit s = putStrLn "Bye!!" >> return (0, s)
    cheer_up s = putStrLn "Cheer up bro!" >> return (0, s)
    getCharAndReact s = getLine >>= (return . split (\x -> read x :: Int) (const s))

{-
Looking sharp, base mission accomplished. So, still lots to do.
1. Check up State Monad and see what we can do with that. Seems very useful here tbh
2. Check up a way to handle errors seemslessly, would be the bomb too
3. As mentioned before, check out ana's, cata's and hylo's. They must be useful. Some ideas in mind, need more testing.
4. Full blown example... A simple gloss platformer game would be perfect. But it's actually looking solid.
-}

-- Simple example testing StateT monad

tick :: StateT Int IO Int
tick = do 
    n <- get
    put (n+1)
    return n

plusOne :: Int -> IO Int
plusOne n = execStateT tick n

-- Let's try rewriting simple_menu using this state and adding some info into it maybe
simple_menu_with_state :: Rose (TState -> IO (Int, TState))
simple_menu_with_state = Rose start [Rose options [Rose quit []], Rose quit [], Rose cheer_up []] where
    start = runStateT start_screen
    options = runStateT options_screen
    quit = runStateT quit_action
    cheer_up = runStateT cheer_up_action

increment_state_counter = do
    (S n) <- get
    put (S (n+1))

inc_get_read f = do
    increment_state_counter
    f
    liftIO getLine >>= (return . read)

start_screen :: StateT TState IO Int
start_screen = inc_get_read $ liftIO $ putStrLn "1. Options" >> putStrLn "2. Quit" >> putStrLn "3. Cheer up!"

options_screen :: StateT TState IO Int
options_screen = inc_get_read $ liftIO $ putStrLn "1. Augment volume" >> putStrLn "2. Decrease volume"

quit_action = do
    increment_state_counter
    liftIO $ putStrLn "Bye"
    return 0

cheer_up_action = do
    increment_state_counter
    liftIO $ putStrLn "Cheer up!"
    return 0


{-
Hmm... Not sure how much power we gained from this construction... I mean, the getCharAndReact above function kinda was doing some sort of state manipulation... We got rid of that explicitly, not awful.
About errors, I think the easiest way to get that covered is simply working with a monad tranformer on state. Not exactly sure how much (or if anything at all) we will need to change runRose... We'll see.
It's actually looking solid. With some abstraction we can do one-liners, that's always neat. Error treatment will be the next step for sure.

Another important feature that could be added (probably will be looked into after errors) is putting a new funtion in each node. Have it in such a way that the node does the following:
First, start by getting some state and manipulating it in some way. Do whatever you need here basically.
On the second function, we would choose where to branch too. This has some serious advantage since it would allow to reuse a LOT of functions. As you can see above, with this pattern we would be shining really.
Should be kinda ez to do that on the rose function, it will basically have two composed functions maybe. Makes sense since we might want to choose the next path according to our current state. 
Or, even way way way better would be allowing some new info to come next to the state. Like the additional information created by each computation. Then we could react to both state and last computation! 
Alright, this will be implented before errors, it seems too good to pass.
-}

{-
Ok, so, what changes? We are currently sitting at -> runRoseMonadic :: Monad m => a -> Rose (a -> m (Int, a)) -> m a.
What changes is that we won't have one function now? Let's think.. We want to be able to get a State and manipulate it in some way, producing a side-value.
Typewise, this means something like a -> (a,b). From there we want to react to that and produce the next step. We also want to keep the new state for the next computations.
So, basically we should have:
act :: a -> (a,b)
react :: (a,b) -> (Int, a)
We need those monadified, so we should rewrite to the following if im not wrong.
act :: a -> (a,b) (No need to monad here since we are just acting on what we already have I think)
react :: (a,b) -> (Int, a) (Hmm, sounds wrong to put monad here tbh. I think what we wanna do is actually had a new funtion, that just produces output. It doesn't changes the state.)
out :: (a,b) -> m (a,b)

Writing this is kinda tricky tho... Should we provide each rose node with these 3 functions? Could be excessive... But let's try it.
-}

data Node a m b = N {
    act :: a -> m (b,a), -- acts on the initial state (runStateT type)
    out :: m (b,a) -> m (), -- Able to output necessary information
    react :: m (b,a) -> m (Int, a) -- reacts to the new state and to the extra information given
}

{-
run_rose_m :: Monad m => a -> Rose (Node m a b) -> m a
run_rose_m state (Rose n l) = do
    let (new, info) = (act n) state
    (index, new1) <- (out n) (new,info) >>= (return . (react n))
    if length l == 0 then return new1 else run_rose_m new1 (l!!(index-1))



Let me tell you, I'm incredibly surprised that work. Commented it just because I'll refactor it.
-}

run_rose_m :: Strong m => a -> Rose (Node a m b) -> m a
run_rose_m state (Rose n l) = do
    ((index, new),_) <- dstr $ split (react n) (out n) (act n state)
    if length l == 0 then return new else run_rose_m new (l!!(index-1))

{-
Looking better, let's give it a shot with the last example.
-}

-- Let's try rewriting simple_menu using this state and adding some info into it maybe
ez_menu :: Rose (Node TState IO ()) -- could not be (), that's the beauty of this, any info would do
ez_menu = Rose start [Rose options [Rose quit []], Rose quit [], Rose cheer_up []] where
    start = N (runStateT increment_state_counter) ez_start_menu get_value_and_choose
    options = N (runStateT increment_state_counter) ez_option_menu get_value_and_choose
    quit = N (runStateT increment_state_counter) (const $ liftIO $ putStrLn "Bye!") do_nothing
    cheer_up = N (runStateT increment_state_counter) (const $ liftIO $ putStrLn "Cheer up!") do_nothing

ez_start_menu :: IO ((), TState) -> IO ()
ez_start_menu s = do
    liftIO $ putStrLn "1. Options" >> putStrLn "2. Quit" >> putStrLn "3. Cheer up!"
    return ()

ez_option_menu :: IO ((), TState) -> IO ()
ez_option_menu s = do
    liftIO $ putStrLn "1. Augment volume" >> putStrLn "2. Decrease volume"
    return ()

get_value_and_choose :: IO ((), TState) -> IO (Int, TState)
get_value_and_choose s = do
    l <- liftIO getLine
    lstr (read l, fmap p2 s)

do_nothing = lstr . split (const 0) (fmap p2)

{-
The code reduction is more than obvious. Pretty cool. Errors coming up next.
-}