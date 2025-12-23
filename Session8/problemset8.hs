f1 :: (Ord a, Num a) => a -> a -> [[Bool]] -> Bool
f1 a b [[True]] = a + b > 1

g :: Num a => (t -> a, t) -> a -> a
g (f,x) y = f x + y

isolate :: Eq a => [a] -> a -> ([a], [a])
isolate [] x = ([], [])
isolate (y:ys) x 
    | x == y = (ls, y:lr)
    | otherwise = (y:ls, lr)
    where (ls,lr) = isolate ys x

triples :: [(a1, a2, a3)] -> ([a1], [a2], [a3])
triples [] = ([],[],[])
triples ((a,b,c):xs) = (a:as, b:bs, c:cs)
            where (as, bs, cs) = triples xs


