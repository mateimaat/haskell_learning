-- 1
data Onion a = Core a | Layer (Onion a)

instance Functor Onion where
    -- fmap :: (a -> b) -> Onion a -> Onion b 
    fmap f (Core a) = Core (f a)
    fmap f (Layer a) = Layer (fmap f a)

instance Applicative Onion where
    -- pure :: a -> Onion a
    pure = Core
    -- (<*>) :: Onion (a -> b) -> Onion a -> Onion b 
    (Core f) <*> a = fmap f a
-- f :: (a -> b)
-- a :: Onion a
    (Layer f) <*> a = Layer (f <*> a)
-- f :: Onion (a -> b)
-- a :: Onion a

instance Monad Onion where
    -- (>>=) :: Onion a -> (a -> Onion b) -> Onion b 
    (Core a) >>= f = f a
-- f :: (a -> Onion b)
-- a :: a
    (Layer a) >>= f = Layer (a >>= f)
-- f :: (a -> Onion b)
-- a :: Onion a

-- 2
-- plop :: Monad m => (a -> m b) -> [a] -> m [b]
plop f [] = return []
plop f (x:xs) = do y <- f x 
                   ys <- plop f xs 
                   return (y:ys)

-- Ex
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

ex1 = plop (safediv 50) [2, 5, 10]
ex2 = plop (safediv 50) [2, 0, 10]