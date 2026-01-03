
-- Problem 1

partition f [] = ([],[])
partition f (x:xs) | f x = (x:l, r)
                | otherwise = (l, x:r)
                where
                    (l,r) = partition f xs


partition' f xs = (l,s)
    where
        l = [x | x <- xs , f x]
        s = [x | x <- xs , not (f x)]


-- Problem 2

data MixedTree a b = Empty | LeafA a | LeafB b | NodeA a (MixedTree a b) (MixedTree a b) | NodeB b (MixedTree a b) (MixedTree a b) deriving (Show, Eq)


t1 :: MixedTree Integer String
t1 = NodeA 4 (NodeB "dudu" (LeafA 2) (LeafA 9)) (NodeB "zyzy" (LeafA 6) (LeafA 7))

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

funcA :: (Eq a, Num a) => a -> a -> [b] -> (b, b)
funcA a b (x:y:xs) | a == b = (x,y)
                | a + 1 == b = (y,x)

funcB :: (Show a, Fractional a) => p -> a -> [Char]
funcB a b = "The half of" ++ show b ++ "is" ++ show (b/2) 

funcC :: [a] -> [a] -> [a]
funcC xs ys = xs ++ ys

funcD :: (t1 -> t2) -> ((b, b) -> t1) -> b -> t2
funcD f a b = f(a(b,b))


-- Problem 4

recall :: (Eq t1, Num t1) => t1 -> t2 -> [t2]
recall 0 _ = []
recall n x = x : recall (n-1) x

lists :: (Eq t, Num t) => t -> [[t]]
lists n = recall n n : lists (n+1)

lists' :: (Eq t2, Num t2, Enum t2) => t2 -> [[t2]]
lists' n = map (\x -> recall x x) [n..]

lists'' :: (Enum t2, Eq t2, Num t2) => t2 -> [[t2]]
lists'' n = [recall x x | x <- [n..]]

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
    pure = Bingo
    (<*>) :: W (a -> b) -> W a -> W b
    Bingo f <*> Bingo x = Bingo (f x)
    
instance Monad W where
    return x = Bingo x
    Bingo x >>= f = f x


wrapadd x xs = do
    y <- xs
    return (x+y)

h xy yy = do
    x <- xy
    y <- yy
    return (x*y)