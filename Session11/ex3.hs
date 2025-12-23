data Aexp = Var String | Num Integer | Plus Aexp Aexp | Mult Aexp Aexp

look ass x = head [v | (y,v) <- ass, y ==x]

eval (Var x) ass = look ass x
eval (Num n) ass = n 
eval (Plus a1 a2) ass = v1 + v2 
                where
                    v1 = eval a1 ass
                    v2 = eval a2 ass
eval (Mult a1 a2) ass = v1 * v2 
                where
                    v1 = eval a1 ass
                    v2 = eval a2 ass



{- instance Monad Maybe where
    Nothing >>= _ = Nothing
    (Just x) >>= f = f x -}

eval' (Var x) ass = look ass (Just x)
eval' (Num n) ass = Just n

{- eval' (Plus a1 a2) ass = case eval' a1 of
                        Nothing -> Nothing
                        Just a1 -> case eval' a2 of
                                Nothing -> Nothing
                                Just a2 -> eval (a1 ass) + eval (a2 ass) -}

