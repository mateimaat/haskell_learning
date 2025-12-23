second :: [a] -> a
second xs = xs !! 1

second2 :: [a] -> a
second2 xs = head ( tail (xs) )

second3 :: [a] -> a
second3 xs = head ( reverse (take 2 xs) )

second4 :: [a] -> a
second4 (x:xs) = head xs

second5 (_ : y : xs) = y 

allbutsecond :: [a] -> [a]
allbutsecond [] = error "List is empty"
allbutsecond (x:xs) = [x] ++ tail(xs)

allbutsecond2 (x:y:xs) = x:xs



midtover :: [a] -> ([a], [a])

midtover xs = (left , right)
           where 
            halfway = length xs `div` 2
            left = take halfway xs 
            right = drop halfway xs


        
iseven n = (n `mod` 2 == 0)
-- isodd n = not(iseven)

reverse1 :: [a] -> a
reverse1 xs = head(reverse(xs))


qsort2 :: (Ord a) => [a] -> [a]

qsort2 [] = []
qsort2 (x:xs) = big ++ [x] ++ small
                 where small = qsort2 [a | a <- xs, a <= x]
                       big   = qsort2 [a | a <- xs, a > x]
