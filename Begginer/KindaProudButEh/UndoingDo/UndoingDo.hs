module UndoingDo where

import Cp

infix 5 >>==

-- The pattern
some_func :: IO String
some_func = do
    x <- return "some value we need"
    putStrLn "whatever" -- any op we need to do
    return $ x ++ " anything works" -- a op with x now

-- We can abstract the pattern easily by preserving the value we obtain out of the first function
--(>>==) :: Strong m => m a -> m b -> (a -> c) -> m c
--(x >>== y) h = h . p1 <$> dstr (x, y)

-- Express the some_func with this new operator
some_func_pf = (return "some value we need" >>== putStrLn "whatever") (++ " anything_works")

-- We can abstract it easily to do a list of operations instead.
(>>*=) :: Strong m => m a -> [m b] -> (a -> c) -> m c
(x >>*= y) h = h . p1 <$> dstr (x, sequence y)

-- Another way to write some_func.
some_func_pf2 = (return "some value we need" >>*= [putStrLn "whatever"]) (++ " anything_works")

-- Also allows us to perform more operations.
another_func_pf = (return "some value we need" >>*= [putStrLn "whatever", putStrLn "great"]) (++ " anything_works")

------------------------------------------------------------- Not as useful as expected -------------------------------------------------------------

-- Another case of pattern abstraction

pattern :: IO (Int, Int)
pattern = do
    x <- return "10"
    y <- return "20"
    return $ (,) (read x) (read y)

pf_pattern :: IO (Int, Int)
pf_pattern = (return "10" >*< return "20") (uncurry (,) . pow2 read)

infix 5 >*<

(>*<) :: Strong m => m a -> m b -> ((a,b) -> c) -> m c
(x >*< y) h = h <$> dstr (x,y)

-- But isn't this a more generic case of the above?? Well, it is, so we can redefine it now.

(>>==) :: Strong m => m a -> m b -> (a -> c) -> m c
(x >>== y) h = (x >*< y) (h . p1)

-- I also believe we can push it a step further by making the functions start with pure values.
-- This is legit only a product of two unpure operations followd by a pure operation on them.
(>~<) :: Strong m => (a -> m b) -> (c -> m d) -> ((b, d) -> e) -> (a,c) -> m e
(f >~< g) h = h <$.> dstr . (f >< g)

-- But there seems to be enough room to keep it growing stronger. It seems like it can be more abstract in some way.