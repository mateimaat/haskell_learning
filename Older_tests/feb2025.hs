import GHC.Internal.System.Posix.Internals (lstat)
import GHC.Internal.Real (infinity)
-- Problem 2

pairm _ [] = []
pairm n (x:xs)
    | n /= x = (n,x) : pairm n xs
    | otherwise = pairm n xs

pairm' n xs = [(n,x) | x <- xs, n /= x]

pairm'' n xs = map (\x -> (n,x)) (filter (/=n) xs)

-- Problem 1

funcA a b = (a>b, a<b, a, b)
-- adhoc polymorphic

funcB :: Num b => (a, [a]) -> ([a], b)
funcB (x,[xs]) = ([x,xs],0)
-- adhoc polymorphic


funcC (x, _) _ _ _ = x
-- parametric polymorphic


funcD :: [Char] -> ([Char], Bool)
funcD ['a'] = (['b'], True)

-- Problem 3

runs :: Eq a => [a] -> [Int]
runs [] = []
runs (x:xs) = count 1 x xs
  where
    count n _ [] = [n]
    count n y (z:zs)
      | y == z    = count (n+1) y zs
      | otherwise = n : count 1 z zs


gep [] [] = []
gep xs [] = []
gep [] ys = []
gep (x:xs) (y:ys) = if x == y then x: gep xs ys else []



----

runss [] = []
runss (x:xs) = count 1 x xs
    where
      count n _ [] = [n]
      count n y (z:zs) 
        | y == z   = count (n+1) y zs 
        | otherwise = n : count 1 z zs



wrapup [] = [[]]
wrapup [x] = [[x]]
wrapup (x:y:ys)
    | x == y = (x:l):ls
    | otherwise = [x]:l:ls
    where
      (l:ls) = wrapup (y:ys)

  

-- Problem 4

data WNTree = Leaf Int | Node Int [WNTree] deriving Show 

t1 = Node 12 [Node 17 [Leaf 1964], Node 484000 [Leaf 50, Node 6 [Leaf 81, Leaf 1], Leaf 42, Leaf 3]]

maxval (Leaf x) = x 
maxval (Node x ts) = maximum (x : map maxval ts)

-- Problem 5

twiceq 0 = 1
twiceq n = 2 * twiceq (n-1)


dublist x = x : next 1 dublist
  where
    next n dublist = twiceq (n) * x : next (n+1) dublist

dublistt n = n : dublistt (2*n)

dublist' x = [x * (2 ^ n) | n <- [0..]]


-- Problem 6

data Age a = New a |Old a deriving Show

instance Functor Age where
  fmap f (New x) = New (f x)
  fmap f (Old x) = Old (f x)

instance Applicative Age where
  pure x = New x
  (Old f) <*> (New x) = Old (f x)
  (Old f) <*> (Old x) = Old (f x)
  (New f) <*> (New x) = New (f x)
  (New f) <*> (Old x) = New (f x)

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
