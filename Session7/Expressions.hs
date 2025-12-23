data Aexp = Val Int | Strr String | Add Aexp Aexp | Mul Aexp Aexp deriving Show 
myExp = Add (Val 2) (Mul (Val 3) (Strr "X"))