data Aexp = Val Int | Strr String | Add Aexp Aexp | Mul Aexp Aexp deriving Show
myExp = Add (Strr "y") (Mul (Val 2) (Strr "x"))

ass :: [(String, Int)]
ass = [("x", 3), ("y", 4)]

look xs y = head [ b | (a,b) <- xs, a == y]

eval (Val n) ass = n
eval (Strr m) ass = look ass m
eval (Add x y) ass = eval x ass + eval y ass
eval (Mul x y) ass = eval x ass * eval y ass