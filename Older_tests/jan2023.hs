import Data.Time.Format.ISO8601 (yearFormat)
-- Problem 1

-- the function uses parametric polymorphism
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f xs = ([x | x <- xs, f x],[x | x <- xs, not (f x)])

partition' f [] = ([],[])
partition' f (x:xs) 
    | f x = (x:as, bs)
    | otherwise = (as, x:bs)
    where (as, bs) = partition' f xs


-- Problem 2

data MixedTree a b = Empty | LeafA a | LeafB b 
    | NodeA a (MixedTree a b) (MixedTree a b) | NodeB b (MixedTree a b) (MixedTree a b) deriving Show

example :: MixedTree Int String
example =
    NodeA 4
        (NodeB "dudu"
            (LeafA 2)
            (LeafA 9))
        (NodeB "zyzy"
            (LeafA 6)
            (LeafA 7))

islayered :: MixedTree a b -> Bool
islayered Empty = True
islayered (LeafA _) = True
islayered (LeafB _) = True
-- more study here
islayered (NodeA _ left right) =
    isB left && isB right && islayered left && islayered right
  where
    isB (LeafB _)   = True
    isB (NodeB _ _ _) = True
    isB _           = False

islayered (NodeB _ left right) =
    isA left && isA right && islayered left && islayered right
  where
    isA (LeafA _)   = True
    isA (NodeA _ _ _) = True
    isA _           = False


-- Problem 3

-- 1

funcA :: (Eq a, Num a) => a -> a -> [b] -> (b, b)
funcA n m (x:y:_) 
    | n == m    = (x, x)  -- Uses Eq
    | n + m == 0 = (x, y)  -- Uses Num (+) and comparison (requires Ord, but we can stick to Eq)
    | otherwise = (y, x)
-- ad hoc and parametric polymorphism

-- 2

funcB :: (Show a, Fractional a) => p -> a -> [Char]
funcB _ val = "The half of " ++ show val ++ " is " ++ show (val / 2)
-- parametric and adhoc polymorphism

-- 3

funcC xs ys = xs ++ ys
-- parametric polymorphism

-- 4

funcD f g x = f(g(x,x))
-- parametric polymorphism


-- Problem 4

-- the lists function is not polymorphic

repeat' 0 _ = []
repeat' k x = x : repeat' (k-1) x 

lists x = repeat' x x : lists (x+1)

lists' x = map (\k -> repeat' k k) [x..]

lists'' x = [replicate k k | k <- [x..]]



-- Problem 5

newtype Funpair a = Fun (Bool -> a, String -> a)

examplee :: Funpair Int
examplee = Fun ((\b -> if b then 1 else 0), (\s -> length s))

instance Functor Funpair where
    fmap f (Fun (g,h)) = Fun (f.g, f.h)


-- Problem 6

data W x = Bingo x deriving Show

instance Functor W where
    fmap f (Bingo x) = Bingo (f x)


instance Applicative W where
    pure x = Bingo x
    Bingo f <*> Bingo x = Bingo (f x)

    
instance Monad W where
    return x = Bingo x
    Bingo x >>= f = f x


wrapadd x (Bingo y) = do
    z <- Bingo (x + y)
    return z

h (Bingo x) (Bingo y) = do
    x <- Bingo x
    y <- Bingo y 
    return (x*y)