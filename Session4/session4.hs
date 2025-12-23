{- A triple (x,y,z) of positive integers is called pythagorean if x^2+y^2 = z^2. Using 
a list comprehension, deine a function pyths that maps an integer n to all such triples
with components in [1..n]. For example >pyths 5 -> [(3,4,5),(4,3,5)]
-}

pyths :: Int -> [(Int, Int, Int)]

pyths a = [(x,y,z)| x<-[1..a], y<-[1..a], z<-[1..a], x^2+y^2 == z^2]

factors :: Int -> [Int]

factors n = [x | x<-[1..n], n `mod` x == 0]

{- A positive integer is perfect if it equals the sum of all of its factors,
excluding the number itself. Using a list comprehension, define a function perfects
that returns the list of all perfect numbers up to a given limit. For example,
> perfects 500 -> [6,28,496]
-} 

perfects :: Int -> [Int]

perfects a = [x | x<-[1..a], sum(factors(x))-x == x]

perfects2 :: Int -> [Int]

perfects2 n = [x | x <- [1..n], x == sum [f | f <- factors x, f /= x]]


perfects3 :: Int -> [Int]

perfects3 n = [l | l <- [1..n], l == sum (init (factors l))]

-- init takes out the last element of the list

{- The scalar product of two lists of integers xs and ys of lenght n is give by the sum 
of the prducts of the corresponding integers: sum xsi * ysi
Using a list comprehension , define a function that returns the scalar product of two lists.
-}

scalprod :: [Int] -> [Int] -> Int

scalprod xs ys = sum[x*y | (x,y) <- zip xs ys]

scalprod2 :: [Int] -> [Int] -> Int

scalprod2 xs ys = sum[xs!!i * ys!!i | i <- [0..(length xs -1)]]

----------------------- exercise sheet

onlytwo :: [a] -> Bool
onlytwo [_, _] = True
onlytwo _ = False

alldots :: [(Int,Int)] -> [(Int,Int)] -> [Int]
alldots xs ys = [ a*c + b*d | (a,b) <- xs , (c,d) <- ys ]
 --  guard  | generators

----------------------- chapter 4
halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
            where n = length(xs) `div` 2


third :: [a] -> a
third xs = head(tail(tail(xs)))

third2 :: [a] -> a
third2 xs = xs!!2

third3 :: [a] -> a
third3 (_:_:x:_) = x
third3 _ = error "List too short"


safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | length(xs) > 0   = tail xs
            | length(xs) == 0   = []

safetail3 :: [a] -> [a]
safetail3 (_:xs) = xs
safetail3 [] = []


(||||) :: Bool -> Bool -> Bool
True |||| True = True
True |||| False = True
False |||| True = True
False |||| False = False

(||) :: Bool -> Bool -> Bool
True  || _ = True
False || b = b

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True

conjunction :: Bool -> Bool -> Bool
conjunction x y =
  if x
    then if y
           then True
           else False
    else False


-- Exercise 1 --

idhead :: Eq a => [(a, a)] -> [Bool]
idhead xs = [ a == b | (a, b) <- xs ]

idhead2 :: Eq a => [(a, a)] -> Bool
idhead2 [] = False
idhead2 ((x,y): _) = x==y


-- Exercise 2 --

pyt :: (Num c, Eq c, Enum c) => c -> [(c,c,c)]
-- pyt :: Int -> [(Int, Int, Int)]
pyt a = [(x,y,z)| x<-[1..a], y<-[x..a], z<-[x+1..a], x^2+y^2 == z^2]
                --pairings          generators ( y z are later generators ),            guard

-- Exercise 3 --


bighead :: Ord a => [a] -> Int
bighead xs = length [y | y <- tail xs, y > head xs]




-- Exercise 4 --

plonk :: Num a => a -> a -> a -> a
plonk x y z = x+y+z

-- Exercise 5 --

isperfect :: Int -> Bool
isperfect n =  sum [x | x <- [1..n-1], n `mod` x == 0 ] == n


-- Exercise 6 -- 

sevens :: Integral a => a -> [a]
sevens n = [f | f <- [1..n-1], f `mod` 7 == 0 ]

-- Exerise 7 --

flop :: [(a,b)] -> [(b,a)]
flop [] = []
flop xs = [ (y,x) | (x,y) <- xs ]