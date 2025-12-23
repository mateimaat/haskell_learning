import Data.Char 
import Distribution.Simple.Utils (xargs)

-- Problem 2

printRec :: [Char] -> IO ()
printRec [] = return ()
printRec (x:xs) = do putChar x 
                     putChar '\n' 
                     printRec xs 


letter :: IO ()
letter = do x <- getLine
            printRec x
            
               
-- Problem 3

putCharLn :: Char -> IO ()
putCharLn c = do putChar c
                 putChar '\n'


letter1 :: IO ()
letter1 = do xs <- getLine
             sequence_ [ putCharLn x | x <- xs]

-- Problem 4

newline :: IO ()
newline = putChar '\n'

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do newline
                           putStrLn "ERROR: The value is not a number!"
                           getDigit prompt


summ :: Int -> Int
summ x = x + summ(x-1)

printnr :: Int -> IO ()
printnr x = do printnr (x-1)
               putChar (intToDigit x)
               putChar '\n'


hugorm :: IO ()
hugorm = do x <- getDigit "How many numbers would you like to add? "
            let s = summ x
            print s



                

