hugorn :: IO ()
hugorn = do putStr "How many numbers would you like to add? "
            input <- getLine
            let x = read input :: Int
            summ <- sumlines x 0
            putStrLn ("The sum is " ++ show summ)


sumlines :: Int -> Int -> IO Int
sumlines 0 total = return total 
sumlines x total = do input <- getLine
                      let num = read input :: Int
                      sumlines (x-1) (total + num)