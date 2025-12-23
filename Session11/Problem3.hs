data Aexp = Var String | Num Integer | Plus Aexp Aexp | Mult Aexp Aexp
ass = [ ("x", 3 ) , ("y", 4 ) ]

look :: String -> [(String, Integer)] -> Maybe Integer
look x ass =
    case [v | (y,v) <- ass, y == x] of
        []    -> Nothing
        (v:_) -> Just v


eval :: Aexp -> [(String, Integer)] -> Maybe Integer
eval (Var x) ass = look x ass
eval (Num n) ass = Just n
eval (Plus a1 a2 ) ass = do x <- eval a1 ass
                            y <- eval a2 ass
                            return (x + y)
eval (Mult a1 a2 ) ass = do x <- eval a1 ass
                            y <- eval a2 ass
                            return (x * y)