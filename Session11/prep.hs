import GHC.Platform.ArchOS (ArmISAExt(VFPv2))
-- Define a function tuple using explicit (>>=) and then using do-notation

tuple :: Monad m => m a -> m b -> m (a, b)
tuple ma mb =
    ma >>= \a ->
    mb >>= \b ->
    return (a, b)

tuple1 :: Monad m => m a -> m b -> m (a, b)
tuple1 ma mb = do
    a <- ma
    b <- mb
    return (a,b)


-- ex 2

-- z >>= (\y -> s y >>= (\_ -> return (f y))


-- Problems
-- 2 

data W x = Bingo x deriving Show 

instance Functor W where
    fmap :: (a -> b) -> W a -> W b
    fmap f (Bingo x) = Bingo (f x)

instance Applicative W where
    pure = Bingo
    (Bingo g) <*> x = fmap g x 

instance Monad W where
    return :: a -> W a
    return = pure
    (>>=) :: W a -> (a -> W b) -> W b
    Bingo x >>= f = f x


wrapadd :: Num b => W b -> W b -> W b 
wrapadd xs ys = do
    x <- xs
    y <- ys
    return (x*y)
