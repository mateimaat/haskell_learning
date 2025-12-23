-- Problem 3

putCharLn :: Char -> IO ()
putCharLn c = do putChar c
                 putChar '\n'


letter1 :: IO ()
letter1 = do xs <- getLine
             sequence_ [ putCharLn x | x <- xs]

letter2 = do
    word <- getLine
    sequence_ [putStrLn [x] | x <- word]