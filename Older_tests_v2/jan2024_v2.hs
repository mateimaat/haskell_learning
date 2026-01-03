import Distribution.Compat.Lens (_1)
import Data.Char (digitToInt)
-- Problem 1

rotate [] = []
rotate (x:xs) = xs ++ [x]

-- this function is parametric polymorphic due to no use of constrained type

allrotates [] = []
allrotates xs = xs : take (length xs - 1) (allrotates (rotate xs))


-- Problem 2

data Tree a = Leaf a | Node (Maybe a) (Tree a) (Tree a) deriving Show

t1 = Node (Nothing) (Node (Just 12) (Leaf 17) (Leaf 484000)) (Leaf 1964)

isfull (Leaf _) = True
isfull (Node Nothing _ _) = False
isfull (Node _ l r) = isfull l && isfull r

preorder (Leaf x) = return [x]
preorder (Node Nothing _ _) = Nothing
preorder (Node (Just x) l r) = do
    lx <- preorder l
    rx <- preorder r
    return (x : lx ++ rx)

-- Problem 3

remove xs ys = [y | y <- ys , not (y `elem` xs)]

remove' [] ys = ys 
remove' _ [] = []
remove' xs (y:ys) 
    | y `elem` xs = remove' xs ys
    | otherwise = y : remove' xs ys


-- Problem 4

newtype WrapString a = WS (a,String) deriving Show

instance Functor WrapString where
    fmap f (WS (x,s)) = WS (f x,s)

instance Applicative WrapString where
    pure x = WS (x, " ")
    WS (f, s1) <*> WS (x, s2) = WS (f x, s1)

instance Monad WrapString where
    return = pure
    WS (x, s1) >>= f = 
        let WS (y, s2) = f x
        in WS (y, s1 ++ s2)
    
pairup ws1 ws2 = do
    x <- ws1
    y <- ws2
    return (x, y)

-- Problem 5

funcA :: (Ord b, Num b) => b -> b -> b -> (b, b)
funcA a b c | a > b && b > c+1 = (a,b)

funcB :: [(Integer, t -> Char)]
funcB = [(1, \n -> 'c')]

funcC :: (t1 -> Bool -> t2) -> t1 -> t2
funcC f x = f x True

funcD :: (Num a, Enum a) => a -> [a]
funcD a = [1..a]

-- Problem 6

naturals = make 1 naturals
    where 
        make n naturals = n : make (n+1) naturals

facs = map fact [0..]
    where
        fact 0 = 1
        fact n = n * fact (n-1)

facs' = 1 : zipWith (*) facs' [1..]