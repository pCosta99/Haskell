module Rose where

import Cp
import List hiding (dl)

data Rose a = RNode a [Rose a]

inRose = uncurry RNode
outRose (RNode a l) = (a,l)

baseRose f g = f >< map g

recRose f = baseRose id f

cataRose a = a . (recRose (cataRose a)) . outRose

anaRose f = inRose . (recRose (anaRose f)) . f

hyloRose a c = cataRose a . anaRose c

--  Finishing rose (because it has end-points unlike the one above)
data FRose a b = Single a | FRNode b [FRose a b] deriving Show

inFRose = either Single (uncurry FRNode)

outFRose (Single a) = i1 a
outFRose (FRNode a l) = i2 (a,l)

baseFRose f g h = f -|- g >< map h

recFRose f = baseFRose id id f

cataFRose a = a . (recFRose (cataFRose a)) . outFRose

anaFRose f = inFRose . (recFRose (anaFRose f)) . f

hyloFRose a c = cataFRose a . anaFRose c

instance Functor (FRose a)
        where fmap f = cataFRose $ inFRose . baseFRose id f id

instance BiFunctor FRose
        where bmap f g = cataFRose $ inFRose . baseFRose f g id

-- Monadic cataFRose, anaFRose and hiloFRose. Can have some interest for queries where the Single's end up in a monad for example.
cataFRoseM g = g .! (dr . recFRose (cataFRoseM g) . outFRose) 

anaFRoseM g = (fmap inFRose . dr . recFRose (anaFRoseM g)) .! g

hyloFRoseM f g = cataFRoseM f .! anaFRoseM g

dr :: Strong m => Either a1 (b, [m a2]) -> m (Either a1 (b, [a2]))
dr = either (return . i1) (fmap i2 . lstr . (id >< sequence))

-- Generic operations on roses and froses

-- Put every Singl into a list.
leafs = cataFRose $ either singl (concat . p2)

-- Some concrete examples using the above. Far less interesting than FileSystem.hs tho

fr_example1 = FRNode 4 [FRNode 10 [Single 3, Single 6], Single 2]

opFRose op base = cataFRose $ either id (uncurry op . (id >< opList))
    where opList = cataList (either (const base) (uncurry op))