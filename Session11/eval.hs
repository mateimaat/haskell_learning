
data Aexp = Var String | Num Integer | Plus Aexp Aexp | Mult Aexp Aexp

look ass x = head [v | (y,v) <- ass, y == x]

-- lookM :: [(String, Integer)] -> String -> Maybe Integer
lookM [] _ = Nothing
lookM ((y,v):ys) x
  | x == y    = Just v
  | otherwise = lookM ys x

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

-- eval' :: Aexp -> [(String, Integer)] -> Maybe Integer
eval' (Var x) ass = lookM ass x
eval' (Num n) _ = Just n
eval' (Plus a1 a2) ass = do
  v1 <- eval' a1 ass
  v2 <- eval' a2 ass
  return (v1 + v2)

eval' (Mult a1 a2) ass = do
  v1 <- eval' a1 ass
  v2 <- eval' a2 ass
  return (v1 * v2)

-- test case
myass :: [(String, Integer)]
myass = [("x",3),("y",4)]

expr1 = Plus (Mult (Num 2) (Var "x")) (Var "y")
expr2 = Plus (Num 2) (Var "z")
expr3 = Var "x"
expr4 = Mult (Var "x") (Var "z")
expr5 = Plus (Num 7) (Mult (Num 2) (Num 5))