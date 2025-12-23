{- prodthree :: Num [a] => [[a]] -> [[a]] -> [[a]] -> [[a]]
prodthree [] [] [] = []
prodthree (x:xs) (y:ys) (z:zs) = pure (:) <*> (x*y*z) <*> prodthree xs ys zs -}


prodthree xs ys zs = pure (\x y z -> x * y * z) <*> xs <*> ys <*> zs


