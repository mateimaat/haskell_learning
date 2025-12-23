import Distribution.Simple.Utils (xargs)
import GHC.Float (Floating(log1mexp))
import GHC.Exts.Heap (ClosureType(FUN_0_1))
import Data.Time.Format.ISO8601 (yearFormat)
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

prod :: Num a => [a] -> a
prod [] = 1
prod (n:ns) = n * prod(ns)

lengthh :: [a] -> Int
lengthh [] = 0
lengthh (_:xs) = 1 + lengthh xs

reversee :: [a] -> [a]
reversee [] = []
reversee (x:xs) = reverse xs ++ [x]

zipp :: [a] -> [b] -> [(a,b)]
zipp [] _ = []
zipp _ [] = []
zipp (x:xs) (y:ys) = (x,y) : zipp xs ys

dropp :: Int -> [a] -> [a]
dropp 0 xs = xs
dropp _ [] = []
dropp n (_:xs) = dropp (n-1) xs

andd :: [Bool] -> Bool
andd [] = True
andd (x:xs) = x && andd xs


concatt :: [[a]] -> [a]
concatt [] = []
concatt (xs:xss) = xs ++ concatt xss

replicatee :: Int -> a -> [a]
replicatee 0 x = []
replicatee n x = x : replicatee (n-1) x

insertt :: Int -> [Int] -> [Int]
insertt x [] = [x]
insertt x (y:ys) = if x<= y then x : y : ys
                    else y : insertt x ys

mergee :: [Int] -> [Int] -> [Int]
mergee [] ys = ys
mergee xs [] = xs
mergee (x:xs) (y:ys) = if x <= y then x : mergee xs (y:ys)
                        else y : mergee (x:xs) ys

initt :: [a] -> [a]
initt (x:xs) | null xs = []
            | otherwise = x : initt xs

replicate2 :: Int -> Int -> [Int]
replicate2 0 _ = []
replicate2 n x = [x] ++ replicate2 (n-1) x

improve :: [a] -> [a]
improve [] = []
improve [x] = [x]
improve (x:_:xs) = [x] ++ improve xs

---- Exercise sheet ----

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev(xs) ++ [x]


descending :: Ord a => [a] -> Bool
descending [] = True
descending [x] = True
descending (x:y:xs) = if y < x  then descending (y:xs)
                else False



isolate :: Eq a => [a] -> a -> ([a], [a])
isolate [] _ = ([],[])
isolate (l:ls) x | l == x = (l1, l:l2)
                 | otherwise = (l:l1, l2)
            where (l1,l2)= isolate ls x
-- we put a recursive call in a where
-- we use a pattern (l1,l2)

wrapup :: Eq a => [a] -> [[a]]
wrapup [] = []
wrapup [x] = [[x]]
wrapup (x:y:ys) = if x == y then (x:l):l1
                else [x]:l:l1
            where  (l:l1) = wrapup(y:ys)


rev3 :: [a] -> [a]
rev3 [] = []
rev3 (x:xs) = rev3 xs ++ [x]


descending3 :: Ord a => [a] -> Bool
descending3 [] = True
descending3 [x] = True
descending3 (x:y:xs) = y <= x && descending3 (y:xs)

isolate3 :: Eq a => [a] -> a -> ([a],[a])
isolate3 [] _ = ([],[])
isolate3 (y:ys) x | y == x = (l1, y:l2)
                | otherwise = (y:l1, l2)
            where (l1,l2) = isolate3 ys x

wrapup2 :: Eq a => [a] -> [[a]]
wrapup2 [] = []
wrapup2 (x:y:ys) = if x == y then (x:l):l1
                else [x]:l:l1 
            where (l:l1) = wrapup(y:ys)


triples :: [(a1, a2, a3)] -> ([a1], [a2], [a3])
triples [] = ([],[],[])
triples ((a,b,c):xs) = (a:as, b:bs, c:cs)
    where (as, bs, cs) = triples xs



--- More problems

oddNum :: Int -> Bool
oddNum x = x `mod` 2 == 1

amy :: (a -> Bool) -> [a] -> Bool
amy _ [] = False
amy p (x:xs)
    | p x = True
    | otherwise = amy p xs

main :: IO ()
main = do
    print (amy oddNum [2, 5, 8, 3, 7, 4])
    print (amy oddNum [2, 8, 42])  



-- Chapter 6 exercises --
--2

sumdown :: Int -> Int
sumdown 0 = 0
sumdown x = x + sumdown (x-1)


--4

euclid :: Int -> Int -> Int
euclid 0 0 = 0
euclid x 0 = x
euclid 0 y = y
euclid x y 
    | x == y   = x
    | x >= y   = euclid (x-y) y
    | otherwise = euclid x (y-x)

--7

merge2 :: Ord a => [a] -> [a] -> [a]
merge2 [] [] = []
merge2 xs [] = xs
merge2 [] ys = ys
merge2 (x:xs) (y:ys)
    | x >= y = y : merge2 (x:xs) ys
    | otherwise = x : merge2 xs (y:ys)

--8

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []     = []
msort [x]    = [x]
msort xs     = merge2 (msort left) (msort right)
  where
    (left, right) = halve xs