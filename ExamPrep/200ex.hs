import Data.Char (digitToInt)
import Distribution.Simple.Utils (withTempDirectoryEx, rawSystemExit)
import GHC.Internal.TH.Lib (safe)
-- Problem 1
-- a)

funcA :: Ord a => (a, a) -> String -> Int
funcA (a,b) "" | a > b = digitToInt '2'
        | otherwise = digitToInt '1'
-- adhoc polymorphism due to use of constrained type

funcB :: Bool -> p -> p
funcB b a | b == True = a
-- parametric polymorphism

funcC :: (Ord a1, Eq a2) => a2 -> a2 -> (a1, a1) -> a1
funcC a b (c,d) | a == b && c > d = c
-- adhoc polymorphism due to use of constrained type

funcD :: Show a1 => [a2] -> a1 -> IO ()
funcD [] a  = print a
-- adhoc and parametric polymorphism

funcE :: ((a1, a1), b) -> [a2] -> ((a1, b) -> [a3]) -> [a3]
funcE ((l1, l2), r) [] f = res
                        where
                            typeList = [l1, l2]
                            res = f (l1, r)
                            last = res
-- Parametric polymorphism

-- Problem 2

madras :: (p -> p -> p) -> p -> p -> p
madras f x y = f xz y
    where
        xz = f x x

-- Problem 3

ispalindrome :: Eq a => [a] -> Bool
ispalindrome xs = xs == reverse xs
-- adhoc polymorphism

ispalindrome' :: Eq a => [a] -> Bool
ispalindrome' [] = True
ispalindrome' [_] = True
ispalindrome' xs = head xs == last xs && ispalindrome' (init (tail xs))

-- Problem 4

prefix :: Eq a => [a] -> [a] -> Bool
prefix [] [] = True
prefix [] xs = True
prefix xs [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys
-- adhoc polymorphic due to the use of constrained type

fwin :: Eq a => [a] -> [a] -> Bool
fwin _ [] = False
fwin xs (y:ys) = prefix xs (y:ys) || fwin xs ys
-- adhoc polymorphic due to the use of constrained type

-- Problem 5

increasing :: Ord a => [a] -> Bool
increasing [] = True
increasing [_] = True
increasing (x:y:ys) = x < y && increasing (y:ys)
-- adhoc polymorphic due to the use of constrained type

increasing' :: Ord a => [a] -> Bool
increasing' (x:xs) = foldr p v s
    where
        p = \(a,b) acc-> a < b && acc
        v = True
        s = zip (x:xs) xs

increasing'' xs = and (zipWith (<) xs (tail xs))

-- Problem 6

norm :: Num t => [t] -> t
norm [] = 0
norm (x:xs) = x ^ 2 + norm xs
-- adhoc polymorphism


norm' :: (Foldable t, Num b) => t b -> b
norm' xs = foldr p v s
    where
        p = \x acc -> x*x + acc
        v = 0
        s = xs

dist [] [] = 0
dist (x:xs) (y:ys) = (x-y)^2 + dist xs ys

dist' xs ys = foldr p v s
    where p = (+) . (\(x,y) -> (x-y)^2)
          v = 0
          s = zip xs ys

-- Problem 7

isolate [] _ = ([],[])
isolate (x:xs) l
    | x == l = (s, x:d)
    | otherwise = (x:s, d)
    where
        (s,d) = isolate xs l
-- adhoc polymorphism due to use of constrained type


isolate' :: Eq a => [a] -> a -> ([a], [a])
isolate' xs l = (s,d)
    where
        s = [x | x <- xs, x /= l]
        d = [x | x <- xs, x == l]

isolate'' :: (Foldable t, Eq a) => t a -> a -> ([a], [a])
isolate'' xs y = foldr p v xs
                where
                    p val (different, equals) =
                        if val == y then (different, val:equals) else (val:different, equals)
                    v = ([], [])


-- Problem 8

triangles = [sum [0..n] | n <- [0..]]


triangles' = 0 : next 1 triangles'
    where 
        next n triangles' = sum [0..n] : next (n+1) triangles'

trianglee = nexttriangle 0 1
    where
        nexttriangle n m = n : nexttriangle (n+m) (m+1)


triangles'' = 0:foldr tri [] [1..]
  where   
    tri x acc = (sum [1..x]):acc

-- Problem 9

cubes = [n^3 | n <- [1..]]

icr n = last [x | x <- [0..n], x^3 < n]

sumcubes :: (Num b, Enum b) => b -> b
sumcubes n = foldr p v s
    where
        p = (+) . (\n -> n^3)
        v = 0
        s = [0..n]

-- Problem 10

-- a) Nesting is polymorphic(a, b can accept every type) 
-- algebraic(because is defined with data) recursive(beacause it refers to itself) data type

-- b) S is a data constructor for the type Nesting

-- c)
data Nesting a b = S ( a , b ) | C ( Nesting a b , Nesting a b )

depth :: (Num t, Ord t) => Nesting a b -> t
depth (S(a,b)) = 0
depth (C(a,b)) = max (1+ depth a) (1+ depth b)

-- Problem 11

compress :: (Eq t1, Num t2) => [t1] -> [(t1, t2)]
compress (x:xs) = encode x 1 xs
    where
        encode x acc [] = [(x, acc)]
        encode x acc (y:ys) | x == y = encode x (acc+1) ys
                            | otherwise = (x, acc) : encode y 1 ys

decompress [] = []
decompress ((a,b):xs) = [a | i <- [1..b] ] : decompress xs
-- both functions are adhoc polymorphic due to use of constrained type


-- Problem 12

data Utterances = Moo | Roar | Baa deriving (Show, Eq)


apart [] = ([],[])
apart (x:xs) | x == Moo || x == Roar = (x:cow, sheep)
            | otherwise = (cow, x:sheep)
            where 
                (cow, sheep) = apart xs


apart' xs = foldr p v s
    where
        p = (\x (cow, sheep) -> if x == Baa then (cow, x:sheep) else (x:cow, sheep))
        v = ([], [])
        s = xs


-- Problem 13

isfun [] = True
isfun ((a,_):xs) = not (a `elem` map fst xs) && isfun xs

unique :: Eq b => [b] -> Bool
unique [] = True
unique (x:xs) = not (x `elem` xs) && unique xs

is11 :: (Eq a, Eq b) => [(a, b)] -> Bool
is11 xs = isfun xs && unique (map snd xs)

-- Problem 14

data Btree a = None | Leaf a | Node a (Btree a) (Btree a)

tree = Node 4 (
              Node 3 (
                      Node 2 (None) (Leaf 1)
                     ) (
                      None
                     )
              ) (
              Node 6 (
                      Leaf 5
                     ) (
                      Leaf 7
                     )
              )

leftpath (Leaf a) = [a]
leftpath None = []
leftpath (Node a left right) = a: leftpath left

-- Problem 15

newtype File = File (String, Int) deriving Show
data Directory = Dir String [File] [Directory] deriving Show

d = Dir "etc" fs sd 
  where 
    fs = [File ("f.txt", 5), File ("m.md", 7)]
    sd = [s1, s2]
    s1 = Dir "sub1" [File ("a.txt", 3)] [s11]
    s2 = Dir "sub2" [] []
    s11 = Dir "subsub1" [File ("b.txt", 6)] []

computesizeFiles [] = 0
computesizeFiles ((File(_, n)):xs) = n + computesizeFiles xs

totalsize (Dir _ fs ds) = (computesizeFiles fs) + computesize ds
  where
    computesize [] = 0
    computesize ((Dir _ files dirs):xs) = (computesizeFiles files) + computesize dirs + computesize xs 

-- Problem 17

readNumber :: String -> Maybe Int
readNumber s = case reads s of
                              [(n, "")] -> Just n
                              x -> Nothing

add a b = do 
           n1 <- a 
           n2 <- b 
           return (n1 + n2)


readAndAdd a b = do 
           n1 <- readNumber a
           n2 <- readNumber b
           return (n1 + n2)

-- Problem 18

newtype Funpair a = Fun ( Bool -> a , String -> a ) 

ex = Fun (\b -> if b then 1 else 0, \"hi" -> 5)

instance Functor Funpair where 
  fmap f (Fun (a, b)) = Fun (f.a, f.b)


-- Problem 19

newtype WrapString a = WS ( a , String ) deriving Show

instance Functor WrapString where
  fmap f (WS ( x , s ) ) = WS ( f x , s )

instance Applicative WrapString where
  pure a = WS (a, "")

  (WS (f, _)) <*> (WS (a, s)) = WS (f a, s)

instance Monad WrapString where 
  return = pure

  (WS (a, s)) >>= f = let WS (b, s1) = f a in WS (b, s)

pairup w1 w2 = do 
                a <- w1
                b <- w2
                return (a, b)



-- Problem 20

data W x = Bingo x deriving Show

instance Functor W where
  fmap f ( Bingo x ) = Bingo ( f x )

instance Applicative W where
    pure = Bingo
    (Bingo f) <*> (Bingo a) = Bingo (f a)


instance Monad W where
  return x = Bingo x
  Bingo x >>= f = f x

wrapadd val bin = do 
                   y <- bin
                   return (val + y)

h binx biny = do 
              x <- binx
              y <- biny
              return (x * y)