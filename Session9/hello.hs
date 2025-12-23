hello :: IO ()
hello = do putStrLn "What is your name?"
           word <- getLine
           putStrLn ("Hello " ++ word)

