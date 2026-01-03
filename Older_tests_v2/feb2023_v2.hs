import Data.Char (digitToInt)
import Distribution.Compat.Lens (_1)
-- Problem 1

norm [] = 0
norm (x:xs) = x*x + norm xs

norm' xs = foldr p v s 
    where
        p = (\n acc -> n*n + acc)
        v = 0
        s = xs

dist [] [] = 0
dist (x:xs) (y:ys) = (x-y)^2 + dist xs ys

-- Problem 2

data TwoThree a = Leaf a | Node2 a (TwoThree a) (TwoThree a) | Node3 a (TwoThree a) (TwoThree a) (TwoThree a) deriving (Show, Eq)

unif = Node2 4 (Node2 4 (Leaf 7) (Leaf 9)) (Node2 12 (Leaf 12) (Leaf 17))
 
nounif = Node2 "papa" (Node2 "yoyo" (Leaf "lala") (Leaf "lala")) (Node3 "dada" (Leaf "nene") (Leaf "fifi") (Leaf "bubu"))

all2 (Leaf _) = True
all2 (Node2 _ l r) = all2 l && all2 r
all2 (Node3 _ _ _ _) = False

all3 (Leaf _) = True
all3 (Node3 _ l m r) = all3 l && all3 m && all3 r 
all3 (Node2 _ _ _) = False

uniform t = all2 t || all3 t


-- Problem 3

funcA :: Ord a => (a, a) -> String -> Int
funcA (a,b) " " | a > b = digitToInt '1'

funcB :: Bool -> p -> p
funcB f a | f == True = a

funcC :: Show a1 => [a2] -> a1 -> IO ()
funcC (x:xs) a = print a

funcD :: ((a1, b1), b2) -> [a2] -> ((a1, b2) -> a3) -> [a3]
funcD ((x, _), b) [_] g = [g (x, b)]

-- Problem 4

readNumber :: String -> Maybe Int
readNumber s = case reads s of 
    [(n,"")] -> Just n 
    x -> Nothing

addNumber xs ys = do
    x <- xs
    y <- ys
    return (x+y)

readAndAdd x y= do
    mx <- readNumber x
    my <- readNumber y
    addNumber (Just mx) (Just my)


-- Problem 5

repeat' :: (Eq t1, Num t1) => t2 -> t1 -> [t2]
repeat' _ 0 = []
repeat' x y = x : repeat' x (y-1)

repeatStrings :: (Eq t1, Num t1) => [a] -> [t1] -> [a]
repeatStrings _ [] = []
repeatStrings [] _ = []
repeatStrings (x:xs) (y:ys) = repeat' x y ++ repeatStrings xs ys

repeatStrings' xs ys = foldr (++) [] (map (\(x,y) -> replicate y x) (zip xs ys))

-- Problem 6

triangles = [sum [0..n] | n <- [0..]]

triangles' = 0 : make 0 1 triangles
    where 
        make n m triangles' = (n+m) : make (n+m) (m+1) triangles'


triangles'' = map (\n -> foldr (+) 0 [1..n]) [0..]

