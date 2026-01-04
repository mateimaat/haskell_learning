wrapup [x] = [[x]]
wrapup (x:y:ys) 
    | x == y = (x:l):ls
    | otherwise = [x]:l:ls
    where
        l:ls = wrapup (y:ys)

wrapup2 [] = []
wrapup2 (x:xs) = (x : (takeWhile (==x) xs)) : wrapup2 (dropWhile (==x) xs)



compress :: (Eq t1, Num t2) => [t1] -> [(t1, t2)]
compress (x:xs) = compute x 1 xs
    where
        compute x acc [] = [(x,acc)]
        compute x acc (y:ys) | x == y = compute x (acc+1) ys
                            | otherwise = (x,acc) : compute y 1 ys