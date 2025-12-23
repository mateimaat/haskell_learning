import Data.List (scanl')
-- Problem 1
{- 
 Below are five types. For each of them, define a Haskell value (which may be a function) that has
this particular type as their most general type.

a) Ord a =>(a, a) −> String −> Integer
b) Bool −> p −> p
c) (Ord a1, Eq a2)=>a2 −> a2 −> (a1, a1)−> a1
d) Show a1 =>[a2] −> a1 −> IO ()
e) ((a1, a1), b) −> [a2] −> ((a1, b)−> [a3]) −> [a3]

Moreover, for each of these four types also indicate if the type involves
• parametric polymorphism only
• overloading (ad hoc-polymorphism) only
• both forms of polymorphism
• no polymorphism

Do not annotate your expressions and values with types. The types must be the ones found by
GHCi using type inference.
-}

-- a) Ord a =>(a, a) −> String −> Integer
funcA :: Ord a => (a, a) -> String -> Integer
funcA (l, r) count | l > r = read count :: Integer
                   | l <= r = read count :: Integer
-- ad-hoc polymorphism

-- b) Bool −> p −> p
funcB :: Bool -> p -> p
funcB cond term = if cond then term else term
-- parametric polymorphism

-- c) (Ord a1, Eq a2)=>a2 −> a2 −> (a1, a1)−> a1
funcC :: (Ord a1, Eq a2) => a2 -> a2 -> (a1, a1) -> a1
funcC xs ys (l,r) | xs == ys = if l > r then l else r
-- ad-hoc polymorphism

-- d) Show a1 =>[a2] −> a1 −> IO ()
funcD :: Show a1 => [a2] -> a1 -> IO ()
funcD [] x = print x
-- parametric and ad-hoc polyorphism

-- e)  ((a1, a1), b) −> [a2] −> ((a1, b)−> [a3]) −> [a3]
funcE :: ((a1, a1), b) -> [a2] -> ((a1, b) -> [a3]) -> [a3]
funcE ((l1, l2), r) [] f = res
                        where
                            typeList = [l1, l2]
                            res = f (l1, r)
                            last = head res
-- Parametric polymorphism



-- Problem 2
{- 
Here is the definition of a Haskell function.
madras (f,x,y) = f ( f x x) y
Give a curried version of madras that has type (t −> t −> t)−> t −> t −> t
-}

madras :: (p -> p -> p) -> p -> p -> p
madras f x y = f fx y
        where fx = f x x

-- Problem 3

{-
A palindrome is a string that is the same written forwards and backwards such as “Otto” or
“Madam”.
The goal of this problem is to write a Haskell function ispalindrome that will determine if a string
of characters is a palindrome.
a) First figure out the type of ispalindrome without using the Haskell system. Is the function
polymorphic? Why? How?
b) Now give two different definitions of the function, one that uses the reverse function and one
that does not.
-}

palin1 s = if s == reverse s
    then return True
    else return False


palin [] = True
palin [_] = True
palin s = first == lst && palin s' 
        where first = head s
              lst = last s
              s' = init (tail s)

-- Problem 4

{-
A list xs = [x1, . . . , xk] is a prefix of the list ys if we have that ys = xs or ys = [x1, . . . , xk, y1, . . . , ym],
that is, if ys consists of at least the elements of xs, possibly followed by more elements. As an
example, [3,4,5] is a prefix of [3,4,5,6,484000] . As another example, [] is a prefix of [True,False].
A list xs is found within the list ys if there exist lists z1 and z2 (one or both of which may be empty)
such that ys = z1 ++xs ++z2. As an example, [3,4,5] is found within [1,2,3,4,5,6,484000] . As
another example, [False ,False ] is found within [True,False,False ,True].

a) Define a function prefix that will tell us if a list is a prefix of another list. Is prefix polymorphic? Why and how?

b) Use prefix to define a function fwin that will tell us if a list is found within another list. Is
fwin polymorphic? Why and how?
-}

prefix [] [] = True
prefix xs [] = False
prefix [] ys = True
prefix (x:xs) (y:ys) = x == y && prefix xs ys
-- adhoc polymorphic due to use of constrained type

fwin _ [] = False
fwin xs (y:ys) = prefix xs (y:ys) || fwin xs ys
-- adhoc polymorphic due to use of constrained type


-- Problem 5

{-
A list l is increasing wrt. some ordering relation < if whenever x appears earlier in l than y, then
x < y . The goal is now to define a Haskell function increasing that will take any list as argument
and tell us if it is increasing.
For instance,
increasing [1,2,7,484000]
should return True. On the other hand,
increasing [”ged”,”abe”,”hest”]
should return False .

a) What should the type of increasing be? Is the function polymorphic? Explain.
b) Define increasing using recursion.
c) Define increasing using suitable higher-order functions

-}

-- a) increasing :: Ord a => [a] -> Bool

increasing [] = True
increasing [x] = True
increasing (x:y:ys) = x < y && increasing (y:ys)

increasing' (x:xs) = foldr p v s
                    where
                        p = \(a,b) acc -> a < b && acc
                        v = True
                        s = zip (x:xs) xs
-- adhoc polymorphism

-- Problem 6

{-
Let l = [x1, . . . , xn] be a list of numbers. The squared norm of l.
As an example, norm [1, 3, 5, 6] = 12 + 32 + 52 + 62 = 71.
Your task is to defined a Haskell function norm that computes the norm of any given list of numbers.
a) What is the type of norm? Is norm a polymorphic function? If yes, explain if and why it is
ad hoc or parametric polymorphic. If no, explain why it is not polymorphic.
b) Give a definition in Haskell of norm that uses recursion.
c) Give a definition in Haskell of norm (called norm’) that uses foldr or foldl .
-}

norm [] = 0
norm (x:xs) = x*x + norm xs
-- adhoc polymorphism

norm' (xs) = foldr p v xs 
    where p = \x acc -> x*x + acc
          v = 0

dist [] [] = 0
dist (x:xs) (y:ys) = (x-y)^2 + dist xs ys

dist' xs ys = foldr p v s
    where p = (+) . (\(x,y) -> (x-y)^2)
          v = 0
          s = zip xs ys

-- Problem 7

{-
The function isolate takes a list l and an element x and returns a pair of two new lists (l1 , l2).
The first list l1 is a list that contains all elements in l , that are not equal to x. The second list l2
is a list that contains all occurrences of x in l.
• isolate [4,5,4,6,7,4] 4 evaluates to ([5,6,7],[4,4,4]) .
• isolate [’ g ’,’ a ’,’ k ’,’ a ’] ’a’ evaluates to ([’ g ’,’ k ’], [’ a ’,’ a ’]).

a) What should the type of isolate be?
b) Is isolate a polymorphic function? If yes, explain what forms of polymorphism are used. If
no, explain why isolate is not polymorphic.
c) Define isolate in Haskell
i. using recursion
ii. using list comprehension
iii. using foldr
-}

-- izolate :: (Eq a) => [a] -> a -> ([a], [a])
-- izolate should be adhoc polymorphic due to use of constrained type variables

izolate [] _ = ([], [])
izolate (x:xs) y | x == y = (different, x:equal)
                 | otherwise = (x:different, equal)
                 where 
                    (different, equal) = izolate xs y

izolate' xs y = (different, equals)
                where
                    different = [x | x <- xs, x /= y]
                    equals = [x | x <- xs, x == y]

isolate'' xs y = foldr p v xs
                where
                    p val (different, equals) =
                        if val == y then (different, val:equals) else (val:different, equals)
                    v = ([], [])


-- Problem 8

{-
A triangular number counts the number of dots arranged in an equilateral triangle. The nth
triangular number is the number of dots in the triangular arrangement with n dots on each side,
and is equal to Pn
k=1 k, that is, the sum of the n natural numbers from 1 to n.
The infinite sequence of triangular numbers, starting with the 0th triangular number, starts as
follows.
0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55 . . .

a) Give a definition in Haskell of the infinite list triangles of triangular numbers that uses list
comprehension only, but not recursion.
b) Give a definition of the infinite list triangles ’ of triangular numbers that uses recursion, but
not list comprehension.
c) Give a definition of the infinite list triangles ’’ of triangular numbers that uses higher-order
functions (but no recursion or list comprehension).
-}

triangles = [sum [0..x] | x <- [0..]]

triangles' = nextTriangles 0 1
        where
            nextTriangles i n = i : nextTriangles (i+n) (n+1)

tricount 0 = 0
tricount n = n + tricount (n-1)
triangles'' = map tricount [0..]


-- Problem 9

{-
The set of perfect cubes is the set of natural numbers that are of the form n^3 for some n ∈ N, i.e.
the infinite set {1, 8, 27, 54, . . .}

a) Using Haskell, define the infinite list cubes whose elements are the perfect cubes. What is the
type of cubes?
b) Given a natural number n, the integral cube root of n is the greatest natural number i such
that i
3 ≤ n. As an example, the integral cube root of 9 is 2, since 23 = 8 but 33 = 27.
Use list comprehension in Haskell to define the function icr that for any natural number will
compute its integral cube root. What is the type of icr?
c) Use foldr to define the function sumcubes that computers the sum of the first n cubes for any
given n. For instance, we should have that sumcubes 3 returns 36.
-}

-- cubes :: (Num a) => [a]
-- adhoc polymorphic due to use of constrained type variables

cubes = map (^3) [0..]

icr n = last [x | x <- [0..n], x^3 < n]

sumcubes n = foldr p v s
    where p = (+) . (\n -> n^3)
          v = 0
          s = [0..n]

-- Problem 10

{-
Here is a small piece of Haskell code for defining nested pairs.
data Nesting a b = S ( a , b ) | C ( Nesting a b , Nesting a b )

a) What is the correct terminology for the Haskell concept of which Nesting is an example?
b) What is the correct terminology for the Haskell concept of which S is an example?
c) Here is a Haskell expression.
C (C( S ( True , True ) ,C( S ( True , True ) , S ( True , F al s e ) ) ) , S ( True , F al s e ) ) 
We say that its nesting depth is 3, since every subexpression is found within no more than 3 nested C’s.
Define a depth function that computes the nesting depth of any expression that has type
Nesting a b for any a and b and give the type of the depth function.
-}

-- a) Nesting is polymorphic(a, b can accept every type) 
-- algebraic(because is defined with data) recursive(beacause it refers to itself) data type

-- b) S is a data constructor for the type Nesting

-- c)
data Nesting a b = S ( a , b ) | C ( Nesting a b , Nesting a b )

depth :: (Num t, Ord t) => Nesting a b -> t
depth (S(a,b)) = 0
depth (C(a,b)) = max (1+ depth a) (1+ depth b)

-- Problem 11

{-
The compression of a list is a list that counts successive elements that are repeated and returns a
list of pairs of the form (x, v) where (x, v) indicates that there are v successive elements that are
x’s. 

For instance, the compression of
[ 1 , 1 , 1 , 2 , 1 , 4 , 4 , 4 , 1 , 1 , 6 , 1 , 6 , 4 , 4 , 4 , 4 , 4 ]
is the list
[ ( 1 , 3 ) , ( 2 , 1 ) , ( 1 , 1 ) , ( 4 , 3 ) , ( 1 , 2 ) , ( 6 , 1 ) , ( 1 , 1 ) , ( 6 , 1 ) , ( 4 , 5 ) ]

and the compression of
[ True , True , True , F al se , True , F al se , F al s e ]
is the list
[ ( True , 3 ) , ( F al se , 1 ) , ( True , 1 ) , ( F al se , 2 ) ]

The compression of an empty list is the empty list itself.

a) Using Haskell, define a function compress that computes the compression of a list. What is
the type of compress? Is the function polymorphic? Justify your answer.
b) If we are given a list of pairs where each pair is of the form (x, v) where v is a natural number,
we can decompress the list such that for every element (x, v) we get v successive copies of
each x. The decompression of [[(1,3) ,(2,1) ,(1,1) ,(4,3) ,(1,2) ,(6,1) ,(1,1) ,(6,1) ,(4,5) ] is
therefore [ 1,1,1,2,1,4,4,4,1,1,6,1,6,4,4,4,4,4] Using Haskell, define a function decompress
that computes the decompression of a list. What is the type of decompress? Is the function
polymorphic? Justify your answer.
-}


compress :: (Eq t1, Num t2) => [t1] -> [(t1, t2)]
compress (x:xs) = encode x 1 xs
    where
        encode x acc [] = [(x,acc)]
        encode x acc (y:ys) | x == y = encode x (acc+1) ys
                            | otherwise = (x, acc) : encode y 1 ys

decompress [] = []
decompress ((a,b):xs) = [a | i <- [1..b]] ++ decompress xs
-- decompress ((a,b):xs) = a : if b == 1 then decompress xs else decompress ((a,b - 1):xs)

-- Problem 12

{-
Cows say Moo and Roar. Sheep say Baa. Below is a conversation between a cow and a sheep
expressed as a list.
[Moo, Roar , Baa ,Moo,Moo, Baa , Roar ]

a) Define a datatype Utterances that expresses the sounds that cows and sheep can make.
b) Use your datatype Utterances to define a function apart using recursion such that apart xs
takes a conversation between a cow and a sheep and splits it into a pair (c, s) where c is a list
with the sounds made by the cow in the correct order and s is a list with the sounds made by
the sheep in the correct order.
As example, we should have that
apart [Moo,Roar,Baa,Moo,Moo,Baa,Roar]
gives us ([Moo,Roar,Moo,Moo,Roar],[Baa,Baa]).
What is the type of apart? Is the function polymorphic?
c) Now your datatype Utterances to define a function apart using foldr.
-}

data Utterances = Moo | Roar | Baa deriving (Show, Eq)

apart xs = pastor xs ([],[])
    where 
        pastor [] (m,s) = (m, s)
        pastor (Moo:xs) (m, s) = pastor xs (Moo:m, s)
        pastor (Roar:xs) (m, s) = pastor xs (Roar:m, s)
        pastor (Baa:xs) (m, s) = pastor xs (m, Baa:s)

apart' xs = foldr (\x (m, s) -> if x == Baa then (m, x:s) else (x:m, s)) ([],[]) xs

-- Exercise 13

{-
A function maps different arguments to different values. In Haskell, we can represent any function
that is only defined for finitely many cases as an association list.
We can define a Haskell function isfun that will take any association list and tell us if it represents
a function.
For instance, isfun [(1,’ a’) ,(2,’ b’) ] should return True whereas isfun [(1,’ a’) ,(1,’ b’) ] should
return False .

a) What should the type of isfun be?
b) Now define isfun in Haskell.
c) We know that a function f is 1-1 if whenever we have x, y with x ̸= y we have that f(x) ̸= f(y).
We can define a Haskell function is11 that will take any association list and tell us if it
represents a function which is also 1-1.
For instance, is11 [(1,’ a’) ,(2,’ b’) ] should return True whereas is11 [(1,’ a’) ,(2,’ a’) ] should
return False .
What should the type of is11 be?
d) Now define is11 in Haskell. It is a good idea to make use of the solutions to the previous
subproblems.
-}

--isfun :: Eq a => [(a,b)] -> Bool
isfun [] = True
isfun ((x,_):xs) =
    not (x `elem` map fst xs) && isfun xs

-- is11 :: (Eq a, Eq b) => [(a, b)] -> Bool

unique :: Eq b => [b] -> Bool
unique [] = True
unique (x:xs) = not (x `elem` xs) && unique xs

is11 :: (Eq a, Eq b) => [(a, b)] -> Bool
is11 xs = isfun xs && unique (map snd xs)



-- Exercise 14

{- 
a) Define a datatype Btree a for binary trees that have nodes whose labels are taken from the type a.

b) The leftmost path in a binary tree is the path that can be obtained by starting at the root and
always following the left child of any node, if one exists. If at some point during this traversal
there is no left child, the path ends. In the tree drawn above, the leftmost path is the sequence
4, 3, 2. Define a function leftpath such that leftpath t gives us the list that represents the
leftmost path in the tree t.
Is leftpath polymorphic? If yes, how is it polymorphic? If no, why not?
-}
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

-- leftpath :: Btree a -> [a]
leftpath None = []
leftpath (Leaf v) = [v]
leftpath (Node v a _) = v:(leftpath a)
-- this function uses ad-hoc polymorphism beacause the type of the function explicity states Btree a
