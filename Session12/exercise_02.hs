data Exp a = Var a | Val Integer | Add (Exp a) (Exp a) | Mult (Exp a) (Exp a) deriving Show


-- instance Functor Onion where
--     -- fmap :: (a -> b) -> Onion a -> Onion b
--     fmap f (Layer o) = Layer (fmap f o)
--     fmap f (Core c) = Core (f c)

instance Functor Exp where
    -- fmap :: (a -> b) -> Exp a -> Exp b
    fmap f (Var v) = Var (f v)
    fmap f (Val v) = Val v
    fmap f (Add exp1 exp2) = Add (fmap f exp1) (fmap f exp2)
    fmap f (Mult exp1 exp2) = Mult (fmap f exp1) (fmap f exp2)
    
-- When would it be useful to think of Exp as a functor? 
-- Whenever we want to modify elements of an expression without understanding its format 
-- Not correct anymore: For example, when changing the numeric base of an expression