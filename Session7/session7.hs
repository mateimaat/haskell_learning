-- Exercises FP 12 --

data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero m     = m
add (Succ n) m = Succ (add n m)

mult :: Nat -> Nat -> Nat
mult Zero m     = Zero
mult (Succ n) m = add (mult n m) n

-- example from the book --

type Pos = (Int,Int)
data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)


-- example from the book --

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- reccursive types --

-- data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

-- Tree --

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- Preparation sheet

data Unary = Z
    | I Unary
    deriving (Show, Eq)

unary2int :: Unary -> Integer
unary2int Z = 0
unary2int (I u) = 1 + unary2int u

fourI :: Unary
fourI = I (I (I (I Z)))

least :: Ord a => Tree a -> a
least (Leaf x)        = x
least (Node l y r) = min y (min (least l) (least r))

t1 :: Tree Int
t1 = Node (Node (Leaf 9) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))


-- Problems sheet --

data Aexp = N Int | Var String | Add Aexp Aexp | Mul Aexp Aexp deriving Show
myExpr = Add (N 2) (Add (Var "x") (Mul (N 9) (N 3))) 