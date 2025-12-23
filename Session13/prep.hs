infinity = 1 + infinity

-- Innermost
--fst (0, infinity) = fst (0, 1 + infinity) = fst (0, 1+(1 + infinity))

-- Outermost
-- fst (0, infinity) = 0

ones = 1 : ones

-- Generating primes --

primes = sieve [2..]

sieve (p:xs) = p : sieve [x | x <- xs, mod x p /=0]



-- Preparation sheet

nsonly n = 0 : [(a*n) | a <- [1..]]

nsonly' n=  mult 0
  where
    mult a = (n*a): mult (a+1)
