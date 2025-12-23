import Distribution.Simple.Utils (xargs)
import Control.Concurrent (getChanContents)
import System.IO
act :: IO (Char, Char)
act = do x <- getChar
         y <- getChar
         return (x,y)

{- getLine :: IO String
getLine = do x <- getChar
             if x == '\n' then
                return []
             else
                do xs <- getLine return (x:xs) -}


{- 
    PRIMITIVES
    ----------

    getChar :: IO Char
    putChar :: Char -> IO ()
    return :: a -> IO a
    getLine :: IO String
    putStr :: String -> IO ()
    putStrLn :: String -> IO ()
-}

-- writing a string to the screen
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x 
                    putStr' xs


-- writing a string and moving to a new line
 {- putStrLn' :: String -> IO ()
putStrLn xs = do putStr' xs
                 putChar '\n' -}


strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show(length xs))
            putStrLn " characters"

-- hangman game
hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putStrLn "Try to guess it: "
             play word

-- 
sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else 
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)


-- reads a single character from the keyboard, without echoing it to the screen
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

-- play is the main loop, which requests and processes guesses until the game ends
play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                  putStrLn "You got it! "
               else 
                  do putStrLn (match word guess)
                     play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]


-- Chapter 10 exercises
-- 1. Redefine putStr :: String -> IO () using a list comprehension and the library function sequence_ :: [IO a] -> IO ().

putStr'' :: String -> IO ()
putStr'' xs = sequence_ [ putChar x | x <- xs ]

-- 2. Nim board using recursion

