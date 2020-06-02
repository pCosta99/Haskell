-- All code developed in this module was created accordingly to the paper "Applicative programming with effects" by Conor McBride and Ross Paterson
-- Link --> http://www.staff.city.ac.uk/~ross/papers/Applicative.html 
module ApplicativeTesting where

import Control.Applicative
import Control.Monad
import Cp

-- Redefining sequence on applicative scenarios (they're synonims)
mySeq :: [IO a] -> IO [a]
mySeq [] = pure []
mySeq (c:cs) = liftA2 (:) c (sequence cs)  

mySeq1 [] = pure []
mySeq1 (c:cs) = (:) <$> c <*> (sequence cs)

mySeq2 [] = pure []
mySeq2 (c:cs) = pure (:) <*> c <*> (sequence cs)

-- This means that liftA2 is just a pattern as:
-- liftA2 f a b = f <$> a <*> b
-- Notice also that fmap (or <$>) can be defined as:
-- f <$> a = pure f <*> a 

-- The pattern above (stop-case) + binary application can be generalized to:
dist1 :: Applicative f => [f a] -> f [a]
dist1 [] = pure []
dist1 (v:vs) = liftA2 (:) v (dist1 vs)

-- An example of a interesting use of dist could be mapping a fail-prone operation across a list of inputs
flakyMap :: (a -> Maybe b) -> [a] -> Maybe [b]
flakyMap f = dist1 . fmap f

-- We can avoid the unnecessary traversal and create traverse, as a higher-order dist, to provide the above pattern.
myTraverse1 :: Applicative f => (a -> f b) -> [a] -> f [b]
myTraverse1 f [] = pure []
myTraverse1 f (x:xs) = liftA2 (:) (f x) (myTraverse1 f xs) 

-- This leaded to creating the Traversable class that acts on a functor
class MyTraversable t where
        myTraverse :: Applicative f => (a -> f b) -> t a -> f (t b)
        dist :: Applicative f => t (f a) -> f (t a)
        dist = myTraverse id

-- 4) Exploring the connection between Monoids and Applicatives
-- Accy is a phantom type because the value it holds has nothing to do with a, yet, it yields the baility to accumulate stuff
newtype Accy o a = Acc{acc :: o}

-- f has no effect in acc
instance Functor (Accy o) where
        fmap f (Acc o) = Acc o

instance Monoid o => Applicative (Accy o) where
        pure _ = Acc mempty
        Acc o1 <*> Acc o2 = Acc (o1 <> o2) 

-- Allows us to accumulate values accoring to a certain pure function
accumulate :: (MyTraversable t, Monoid o) => (a -> o) -> t a -> o
accumulate f = acc . myTraverse (Acc . f)

-- Somewhat a concat with no effect other than concating
reduce :: (MyTraversable t, Monoid o) => t o -> o
reduce = accumulate id

-- Flattening a tree becomes as simple as flatten = accumulate (:[])
-- Concat, as mentioned above, could be implemented as concat = reduce

-- With the above abstractions we can now build others on top of it
-- Might and Musty are abstractions that allow to accumulate logical values
newtype Mighty = Might {might :: Bool}

instance Semigroup Mighty where
        Might x <> Might y = Might (x || y) 

instance Monoid Mighty where
        mempty = Might False

newtype Musty = Must {must :: Bool}

instance Semigroup Musty where
        Must x <> Must y = Must (x && y) 

instance Monoid Musty where
        mempty = Must True

-- Instance MyTraversable for lists just for allowing some testing
instance MyTraversable [] where
        myTraverse f [] = pure []
        myTraverse f (x:xs) = liftA2 (:) (f x) (myTraverse f xs) 

-- We can now define the any operation on a MyTraversable instance using Mighty
any :: MyTraversable t => (a -> Bool) -> t a -> Bool
any p = might . accumulate (Might . p)

-- Elem for any instance of MyTraversable
genElem :: (Eq a) => MyTraversable t => a -> t a -> Bool
genElem a = ApplicativeTesting.any ((==) a)

-- We can now define the any operation on a MyTraversable instance using Musty
all :: MyTraversable t => (a -> Bool) -> t a -> Bool
all p = must . accumulate (Must . p)

-- Applicative vs Monad to act on the result of a computation
iffy :: Applicative f => f Bool -> f a -> f a -> f a
iffy = liftA3 cond where
        cond b t e = if b then t else e

miffy :: Monad m => m Bool -> m a -> m a -> m a
miffy mb mt me = do
        b <- mb
        if b then mt else me

-- Applicatives are closed under composition which means we can do the following
newtype Fog f g a = Comp {comp :: (f (g a))}

instance (Functor f, Functor g) => Functor (Fog f g) where
        fmap f (Comp a) = Comp $ fmap (fmap f) a

instance (Applicative f, Applicative g) => Applicative (Fog f g) where
        pure x = Comp $ pure $ pure x
        Comp fs <*> Comp xs = Comp $ liftA2 (<*>) fs xs

-- This should allow to define Accy o as the composition of two applicative functors derived from monads
-- TODO: ???

-- Accumulating exceptions!!
data Except err a = OK a | Failed err

instance (Show a, Show err) => Show (Except err a) where
        show (OK a) = show a
        show (Failed err) = show err 

instance Functor (Except err) where
        fmap f (OK a) = OK $ f a
        fmap f (Failed err) = Failed err 

instance Monoid err => Applicative (Except err) where
        pure = OK
        OK f <*> OK x = OK (f x)
        OK f <*> Failed err = Failed err
        Failed err <*> OK x = Failed err
        Failed err1 <*> Failed err2 = Failed (err1 <> err2)

-- Some examples
-- Evaluates something into an exception, not sure if it's a useful example
evaluator :: (b -> Bool) -> (b -> d) -> (b -> c) -> b -> Except [c] d
evaluator p d s = cond p (pure . d) (Failed . singl . s)

-- Performing safe division with Except
safeDivide :: Float -> Float -> Except [String] Float
safeDivide a b = if (b /= 0) then pure (a/b) else Failed $ singl $ "Can't divide " ++ show a ++ " by 0!"

-- Safe division expressed through evaluator
evalSafeDivide :: Float -> Float -> Except [String] Float
evalSafeDivide = (evaluator ((/= 0) . p2) (uncurry (/)) (\(x,y) -> show x ++ " can't be divided by 0!") .) . (,)

-- Divide every pair of a list
safeDivisionAcrossList :: [Float] -> [Float] -> Except [String] [Float]
safeDivisionAcrossList = (myTraverse (uncurry safeDivide) .) . zip

-- SIDE NOTE: Exploring point freeing double entry functions

-- Just noticed this is pretty much what curry does. We want to pointfree curry to start
no_pf_random_func :: ((a,b) -> c) -> a -> b -> c
no_pf_random_func f a b = f $ (,) a b
no_pf_random_func1 f a b = f `Cp.ap` (,) a b

-- Let's start by removing one of the explicit parameters
-- Nothing to fancy, the pair will be created by joining b with (,) a and then it is composed with f
semi_pf_random_func :: ((a,b) -> c) -> a -> (b -> c)
semi_pf_random_func f a = f . (,) a
semi_pf_random_func1 f a = (.) f $ (,) a

-- Now it gets tricky. At first glance one might expect that to remove another parameter we can just kick a out and go with f . (,)
-- Turns out we can't as I just found out! Because of the following:
-- Haskell is reading our function as func :: ((a,b) -> c) -> a -> (b -> c). We provide it with a function and a value and it gives us a partial function.
-- The problem with this is that once we remove another parameter we are sitting on func :: ((a,b) -> c) -> (a -> (b -> c)). We feed it a function and gives us a partial function that will gives us yet another partial function.
-- So, what's happening here is the following:
-- (,) gets fed A  giving us --> (A x B) ^ B
-- (f .) is then fed through composition with the partial function obtained above, giving us --> B ^ C
almost_pf_random_func :: ((a,b) -> c) -> (a -> (b -> c))
almost_pf_random_func f = (f .) . (,)
almost_pf_random_func1 f = (.) (f .) (,)

-- The full pointfreeee! Easy enough to break down in a similar approach.
-- We now start with a C ^ (A x B) that we will feed to (.) which will give us --> 
-- This is out of my league for now...
-- 2 hours later it's still very out of my league. The above is cake now though.
damm_this_is_scary :: (((a,b) -> c) -> (a -> (b -> c)))
damm_this_is_scary = (. (,)) . (.)
damm_this_is_scary1 = (.) (. (,)) (.)