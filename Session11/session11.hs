import GHC.Internal.TH.Syntax (Exp, nameBase)
{- class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f(a -> b) -> f a -> f b -}
    
-- Monads : a simple evaluator

data Expr = Val Int | Div Expr Expr

e :: Expr
e = Div (Val 6) (Val 3)

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

-- How to rewrite

safediv :: Int -> Int -> Maybe Int 
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval' :: Expr -> Maybe Int
eval' (Val n) = Just n
eval' (Div x y) = case eval' x of
                    Nothing -> Nothing
                    Just n -> case eval' y of
                        Nothing -> Nothing
                        Just m -> safediv n m


-- Using applicators

{- eval'' :: Expr -> Maybe Int
eval'' (Val n) = Just n
eval'' (Div x y) = safediv <$> eval'' x <*> eval'' y -}

{- mx >>= f = case mx of
        Nothing -> Nothing
        Just x -> f x -}

eval'' :: Expr -> Maybe Int
eval'' (Val n) = Just n
eval'' (Div x y) = eval'' x >>= (\n -> eval'' y >>= (\m -> safediv n m))

evaal :: Expr -> Maybe Int
evaal (Val n) = Just n
evaal (Div x y) = do n <- evaal x
                     m <- evaal y
                     safediv n m


-- Monad

{- class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a 
    return = pure -}

-- Maybe example

{- instance Monad Maybe where
    Nothing >>= f = Nothing
    Just x >>= f = f x -}

-- List example

{- instance Monad [] where
    -- (>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>= f = concat map f xs
          or [y | x <- xs, y <- f x] -}

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x,y)

-- State Monad

{- type State = ...
type ST a = State -> (a, State) -

newtype ST a = S (State -> (a,State))

appp :: ST a -> State -> (a, State)
appp (S st) s = st s.  -}

