import GHC.Internal.TH.Lib (safe, parS)
import Data.Char (digitToInt)
-- Problem 1
-- norm is ad-hoc polymorphic

norm [] = 0
norm (x:xs) = x*x + norm xs

norm' xs = foldr p v s 
    where
        p x acc = x*x + acc
        v = 0
        s = xs

dist [] [] = 0
dist (x:xs) (y:ys) = (x-y)^2 + dist xs ys


-- Problem 2

data TwoThree a = Leaf a |
    Node2 a (TwoThree a) (TwoThree a) |
    Node3 a (TwoThree a) (TwoThree a) (TwoThree a) deriving Show

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

funcA (a,b) s
    | a > b && s == "String" = digitToInt '7'

-- adhoc polymorphism

funcB :: Bool -> p -> p
funcB b x = if b then x else x

-- parametric polymorphism


funcC :: Show a1 => [a2] -> a1 -> IO ()
funcC [_] x = print x

-- parametric and ad-hoc polymorphism



funcD :: ((a1, b1), b2) -> [a2] -> ((a1, b2) -> a3) -> [a3]
funcD ((x, _), b) [_] g = [g (x, b)]

-- parametric polymorphism


-- Problem 4

readNumber :: String -> Maybe Int
readNumber s = case reads s of 
    [(n,"")] -> Just n 
    x -> Nothing


add :: (Monad m, Num b) => m b -> m b -> m b
add mx my = do
    x <- mx
    y <- my
    return (x+y)

readAndAdd :: String -> String -> Maybe Int
readAndAdd x y= do
    mx <- readNumber x
    my <- readNumber y
    add (Just mx) (Just my)


-- Problem 5

-- parametric polymorphism

repeatStrings _ [] = []
repeatStrings [] _ = []
repeatStrings (x:xs) (n:ns) = replicate n x ++ repeatStrings xs ns

repeatStrings2 xs ns = foldr (++) [] (map (\(x,n) -> replicate n x) (zip xs ns))

-- Problem 6

triangles = [sum [0..n] | n <- [0..] ]


triangles' = 0 : next 1 triangles'
    where
        next n (t:ts) = (t + n) : next (n + 1) ts


triangles'' = map (\n -> foldr (+) 0 [1..n]) [0..]