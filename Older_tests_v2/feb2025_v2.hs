import GHC.Internal.TH.Lib (safe)
import Distribution.Simple.Utils (xargs)
-- Problem 1

funcA :: Ord d => d -> d -> (Bool, Bool, d, d)
funcA a b = (a>b , b>a, a ,b)

funcB :: Num b => (a, [a]) -> ([a], b)
funcB (a, xs) = (a:xs , 1)


funcC :: (a, b) -> p1 -> p2 -> p3 -> a
funcC (x, _) _ _ _ = x

funcD :: [Char] -> ([Char], Bool)
funcD ['a'] = (['b'], True)


-- Problem 2

pairm _ [] = []
pairm n (x:xs) 
    | n == x = pairm n xs
    | otherwise = (n,x) : pairm n xs

pairm' :: Eq b => b -> [b] -> [(b, b)]
pairm' n xs = [(n,x) | x <- xs, n /= x]

pairm''' :: Eq b => b -> [b] -> [(b, b)]
pairm''' n xs = map (\x -> (n,x)) (filter (/=n) xs)


-- Problem 3

runs [] = []
runs (x:xs) = rep 1 x xs
    where
        rep n _ [] = [n]
        rep n x (y:ys)
            | x == y = rep (n+1) x ys
            | otherwise = n : rep 1 y ys
        
gcp [] [] = []
gcp _ [] = []
gcp [] _ = []
gcp (x:xs) (y:ys) = if x == y then x : gcp xs ys else []


-- Problem 4

data WNTree = Leaf Int | Node Int [WNTree] deriving Show

t1 :: WNTree
t1 = Node 12 [Node 17 [Leaf 1964], Node 484000 [Leaf 50, Node 6 [Leaf 81, Leaf 1], Leaf 42, Leaf 3]]

maxval :: WNTree -> Int
maxval (Leaf x) = x
maxval (Node x args) = maximum (x : map maxval args)


-- Problem 5

twice :: (Eq t1, Num t1, Num t2) => t1 -> t2
twice 0 = 1
twice n = 2 * twice (n-1)

dublist :: Num a => a -> [a]
dublist n = n : mult 1 n dublist
    where
        mult m n dublist = ((twice m) * n) : mult (m+1) n dublist

dublistt :: Num t => t -> [t]
dublistt n = n : dublistt (2*n)

dublist' :: Integral b => p -> [b]
dublist' n = [n * (2 ^ n) | n <- [0..]]


-- Problem 6

data Age a = New a |Old a deriving Show

instance Functor Age where
    fmap f (New x) = New (f x)
    fmap f (Old x) = Old (f x)

instance Applicative Age where
    pure x = New x
    (New f) <*> (Old x) = New (f x)
    (New f) <*> (New x) = New (f x)
    (Old f) <*> (New x) = Old (f x)
    (Old f) <*> (Old x) = Old (f x)

instance Monad Age where
    return = pure
    New x >>= f = f x
    Old x >>=  f = case f x of
      New y -> Old y
      Old y -> Old y


rejuvenate x y = do 
  xi <- x
  yi <- y
  return (min xi yi)