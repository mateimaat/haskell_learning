-- ex1

x = 1 : (map (1+) x)


-- ex2

fibsfrom n1 n2 = n1 : (fibsfrom n2 (n1+n2))

-- sum ( take 50 (fibsfrom 0 1))

-- ex3

indflet :: a -> [a] -> [a]
indflet _ [] = [] 
indflet _ [x] = [x]
indflet e (x:y:ys) = x : e : indflet e (y:ys)

-- ex4
