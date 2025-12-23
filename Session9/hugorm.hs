hugorm :: IO()
hugorm = do putStr "How many numbers would you like to add? "
            input <- getLine
            let num = read input :: Int
            res <- readAndSum num 0
            putStrLn ("The sum is " ++ show res)


readAndSum :: Int -> Int -> IO Int
readAndSum 0 total = return total
readAndSum n total = do 
                        input <- getLine
                        let num = read input :: Int
                        readAndSum (n-1) (total + num)
