class (Functor m) => MyMonad m where
    return :: a -> m a
    join :: m (m a) -> m a
    bind :: (a -> m b) -> (b -> m c) -> a -> m c
    bind f g = join . fmap g . f 

instance MyMonad [] where
        return x = [x]
        join = concat

instance MyMonad Maybe where
        return = Just        
        join (Just (Just x)) = Just x
        join _ = Nothing

fun1 :: Int -> Maybe Int
fun1 x = if (x > 5) then Just (x+4) else Nothing

fun2 :: Int -> Maybe String
fun2 x = if (x > 10) then Just $ show x else Nothing 

double_fun :: Int -> Maybe String
double_fun = bind fun1 fun2