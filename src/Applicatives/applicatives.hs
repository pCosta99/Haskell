-- All code developed in this module was created accordingly to the paper "Applicative programming with effects" by Conor McBride and Ross Paterson
-- Link --> http://www.staff.city.ac.uk/~ross/papers/Applicative.html 
module ApplicativeTesting where

import Control.Applicative
import Control.Monad

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
evaluator p a = if (p a) then OK a else Failed "shit"  
