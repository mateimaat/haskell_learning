-- Problem 1

descending [] = True
descending [_] = True
descending (x:y:ys) = x >= y && descending (y:ys)

-- adhoc polymorphic


segments :: Eq a => [a] -> [[a]]
segments [] = []
segments (x:xs) = (x : takeWhile (== x) xs)
                  : segments (dropWhile (== x) xs)

wrapup :: Eq a => [a] -> [[a]]
wrapup [] = []
wrapup [x] = [[x]]
wrapup (x:y:ys) 
    | x == y = (x:l):l1 
    | otherwise = [x]:l:l1 
    where (l:l1) = wrapup (y:ys)

-- adhoc polymorphic

-- Problem 2

data Encyclopedia a = Leaf String a | Node String a [Encyclopedia a] deriving Show

t1 = Node "mango" True [Node "dingo" False [Leaf "plip" True, Leaf "ninka" False], Leaf "plop" True, Node "plys" False [Leaf "boing" True]]


containskey k (Leaf x _) = x == k
containskey k (Node x _ children) = k == x || any (containskey k) children


value :: Encyclopedia a -> a
value (Leaf _ v)     = v
value (Node _ v _ )  = v


layered :: Ord a => Encyclopedia a -> Bool
layered (Leaf _ _) = True
layered (Node _ v cs) =
    all (\c -> value c > v && layered c) cs



-- Problem 3

interleave _ [] = []
interleave [] _ = []
interleave (x:xs) (y:ys) = x : y : interleave xs ys


interleave' xs ys = concat (map (\(x,y) -> [x,y]) (zip xs ys))


-- Problem 6

harmonic :: [Double]
harmonic =  make 1 harmonic
    where
        make n harmonic = 1 / n : make (n+1) harmonic

harmonic' = map (\n -> 1/n) [1..]

psum 0 = 0
psum n = 1/n + psum (n-1)

psum' x = sum ( map (\n -> 1/n) [1..x] )


-- Problem 5

funcA a b = ([a,a], b+1)
-- adhoc polymorphic


funcB :: (t1 -> t2 -> t1 -> t3) -> t1 -> t2 -> t3
funcB f a b = f a b a
-- parametric polymorphic

funcC = Just []
-- parametric polymorphic

func4' :: (t1 -> a) -> t1 -> (t2 -> a) -> t2 -> Int
func4' f x g y = length [f x, g y]

-- parametric polymorphic

-- Problem 4

data PExp = Var String | Num Int | Add PExp PExp

type State = String -> Maybe Int

s0 :: State
s0 "x" = Just 4
s0 "y" = Just 5
s0 _   = Nothing


eval :: PExp -> State -> Maybe Int
eval (Num n) _ = Just n
eval (Var x) s = s x
eval (Add e1 e2) s = do
    v1 <- eval e1 s
    v2 <- eval e2 s
    return (v1 + v2)


