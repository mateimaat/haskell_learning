wrapup [x] = [[x]]
wrapup (x:y:ys) 
    | x == y = (x:l):ls
    | otherwise = [x]:l:ls
    where
        l:ls = wrapup (y:ys)

wrapup2 [] = []
wrapup2 (x:xs) = (x : (takeWhile (==x) xs)) : wrapup2 (dropWhile (==x) xs)