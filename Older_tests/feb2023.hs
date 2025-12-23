import GHC.Internal.TH.Lib (safe)
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

