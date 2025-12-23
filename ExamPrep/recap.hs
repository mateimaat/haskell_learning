import GHC.Internal.TH.Lib (fromE, safe)


-- Session 2

second xs = head (tail xs)
second' (x:y:ys) = y

-- Session 3

quango x = [x]
tango :: Num a1 => (a2, b) -> p -> a1
tango (_,_) _ = 4

thesame [] = []
thesame ((a,b):xs)
    | a == b = (a,b): thesame xs
    | otherwise = thesame xs

facem x y (a,b) = if a > b && x == y then a
    else b


-- Session 4

onlytwo [_,_] = True
onlytwo _ = False

alldots xs ys = [ a*c + b*d | (a,c) <- xs , (b,d) <- ys]

idhead [] = False
idhead ((a,b):xs) = a == b

pyt n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a*a + b*b == c*c]

bighead :: Ord a => [a] -> Int
bighead xs = length [y | y <- tail xs, y > head xs]

isperfect n = n == sum [y | y <- [1..n-1] , n `mod` y == 0]

sevens k = [y | y <- [1..k-1] , y `mod` 7 == 0]

flop [] = []
flop xs = [(y,x) | (x,y) <- xs]

dupli [] = []
dupli (x:xs) = x:x: dupli xs

-- Session 5

replicate' 0 _= []
replicate' n x = x : replicate' (n-1) x

improve [] = []
improve [x] = [x]
improve (x:y:ys) = [x] ++ improve ys

reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]

descending [] = True
descending [x] = True
descending (x:y:ys)
    | x < y = descending (y:ys)
    | otherwise = False


isolate [] _ = ([],[])
isolate (l:ls) x
    | x == l = (necor, l:cor)
    | otherwise = (l:necor, cor)
    where (necor, cor) = isolate ls x

wrapup [] = []
wrapup [x] = [[x]]
wrapup (x:y:ys) 
    | x == y = (x:l):l1 
    | otherwise = [x]:l:l1 
    where (l:l1) = wrapup (y:ys)


triples [] = ([],[],[]) 
triples ((a,b,c):xs) = (a:as, b:bs, c:cs)
    where
        (as,bs,cs) = triples xs


rle :: Eq a => [a] -> [(a, Int)]
rle [] = []
rle (x:xs) = (x, count) : rle rest
  where
    count = 1 + length same
    (same, rest) = span (== x) xs

amy f [] = False
amy f (x:xs) = f x || amy f xs


frequencies [] = []
frequencies (x:xs) = (x, count) : frequencies oth
    where 
        count = 1 + length (filter (==x) xs)
        oth = filter (/=x) xs

--  Session 6

sumsq x = foldr p v s
    where
        p = (+) . (\n -> n*n)
        v = 0
        s = [1..x]

dbs = filter (\(x,y) -> y == 2*x)

within xs (a,b) = filter (\x -> x >= a && x <= b) xs

sumrows xss = foldr (\row acc -> sum row : acc) [] xss

fact k = product [1..k]
aprox n = sum (map (\x -> 1/fact x) [0..n])

aprox' n = sum xs 
        where
            xs = [1/fact x | x <- [0..n]]


partition p xs = (filter p xs, filter (not . p ) xs)

partition' p = foldr select ([],[])
    where
        select x (ts, fs)
            | p x = (x:ts, fs)
            | otherwise = (ts, x:fs)


remove xs = foldr (\c acc -> if c `elem` xs then acc else c : acc) []

min2 :: Ord a => [a] -> a
min2 xs = minimum xs'
  where
    m  = minimum xs
    xs' = removeFirst m xs

-- Helper to remove the first occurrence of an element
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst y (z:zs)
    | y == z    = zs
    | otherwise = z : removeFirst y zs


data Tree a = Leaf a | Node (Tree a) a (Tree a)

t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

least :: Tree a -> a
least (Leaf a) = a
least (Node l val r) = least l

-- otherwise we need to find the minimum
t' = Node (Node (Leaf 3) 1 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

least' :: Ord a => Tree a -> a
least' (Leaf a) = a
least' (Node l x r) = minimum [least' l, x, least' r]

data Aexp = Val Int | Str String | Add Aexp Aexp | Mul Aexp Aexp deriving Show

look xs y = head [ b | (a,b) <- xs, a == y]

eval (Val n) ass = n
eval (Str m) ass = look ass m
eval (Add x y) ass = eval x ass + eval y ass
eval (Mul x y) ass = eval x ass * eval y ass

{-
We say that a binary tree is balanced if the number of leaves in every left and right subtree differ by
at most one with leaves themselves being trivially balanced. Define a function balanced that will tell
us if a binary tree is balanced or not. Hint: It is a good idea to also define a function that finds the
number of leaves of a tree.
-}

numLeaves (Leaf _)     = 1
numLeaves (Node l val r) = numLeaves l + numLeaves r

balanced (Leaf _) = True
balanced (Node l val r) =
  balanced l && balanced r && abs (numLeaves l - numLeaves r) <= 1


-- Session 8

data Box a = Red (Box a) (Box a) | Blue a deriving (Show,Eq)

samey :: Eq a => Box a -> Bool
samey (Blue _) = True
samey (Red b1 b2) = samey b1 && samey b2 && check b1 b2
  where
    check (Blue x) (Blue y) = x == y
    check _ _               = True


-- Session 9
putCharLn :: Char -> IO ()
putCharLn c = do putChar c
                 putChar '\n'

letter1 :: IO ()
letter1 = do xs <- getLine
             sequence_ [ putCharLn x | x <- xs]


printRec :: [Char] -> IO ()
printRec [] = return ()
printRec (x:xs) = do putChar x 
                     putChar '\n' 
                     printRec xs 


letter :: IO ()
letter = do x <- getLine
            printRec x

            hugorm :: IO()
hugorm = do putStr "How many numbers would you like to add? "
            input <- getLine
            let num = read input :: Int
            res <- readAndSum num 0
            putStrLn ("The sum is " ++ show res)


readAndSum :: Int -> Int -> IO Int
readAndSum 0 total = return total
readAndSum n total = do 
                        input <- getLine
                        let num = read input :: Int
                        readAndSum (n-1) (total + num)


sumInts :: Integer -> IO Integer
sumInts total = do
    putStrLn "Enter a number (0 to stop):"
    line <- getLine
    let n = read line :: Integer
    if n == 0
        then return total
        else sumInts (total + n)
