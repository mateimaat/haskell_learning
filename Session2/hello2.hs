hello :: IO ()
hello = putStrLn "Hello World!"

numberSquare :: IO ()
numberSquare = do
    let number = 5
    let square = number * number
    putStrLn ("Patratul numarului 5 este: " ++ show square)

main :: IO ()
main = do
    hello
    numberSquare
