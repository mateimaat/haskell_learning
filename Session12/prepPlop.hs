plop :: Monad m => (a -> m b) -> [a] -> m [b] -- same type as mapM

plop f [] = return [] -- basic case
plop f (x:xs) = do
  y <- f x -- applies f to first element, x, and gives a monadic result (e.g. just b, IO b, etc.). if its a success,
           -- the result (b) binds to y.
  ys <- plop f xs -- recursively processes the tail, xs :: [a], which produces m [b]. if its a success,
                  -- the result (list [b]) binds to ys.
  return (y:ys)

-- Summary: Apply f to each element of the list, in order, collecting all results in a list, but inside a monad.
-- The use of "do" makes it a monad, which is basically a readable notation to replace chained binds.

-- test case
-- plop (\x -> Just (x + 1)) [1,2,3]