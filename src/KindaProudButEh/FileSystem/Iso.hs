module Iso where

-- Simple module that allows us to put functions in a isomorphism in a very clean way.
-- The most interesting case is when a function creates an isomorphism with itself but we are flexible enough to also provide an operation when this isn't the case.

type Iso a b = (a -> b, b -> a)

type SelfIso a = a -> a

infix 5 >=<

-- enclose for a isomorphism created from a single function
encloseS :: SelfIso a -> (a -> a) -> a -> a
encloseS iso f = iso . f . iso

(>=<) = encloseS

enclose :: Iso a b -> (b -> b) -> a -> a
enclose (fi,gi) h = gi . h . fi