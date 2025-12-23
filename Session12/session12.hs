data Onion a = Core a | Layer (Onion a)
  deriving (Show, Eq)

{- instance Functor Onion where
    -- fmap :: (a -> b) -> Onion a -> Onion b
    fmap f (Core x)    = Core (f x)
    fmap f (Layer o) = Layer (fmap f o)

instance Applicative Onion where
    -- pure :: a -> Onion a
    pure = Core
    -- (<*>) :: Onion (a -> b) -> Onion a -> Onion b
    Core f   <*> o = fmap f o
    Layer fO <*> o = Layer (fO <*> o)

instance Monad Onion where
    Core x   >>= f = f x
    Layer o  >>= f = Layer (o >>= f) -}

onionA = Layer(Layer(Core 1))

onionB = Layer(Core 2)

onionPair a b = do
    cA <- a
    cB <- b
    return(cA, cB)


instance Functor Onion where
    fmap f (Core a) = Core (f a)
    fmap f (Layer o) = Layer ( fmap f o)

instance Applicative Onion where
    pure = Core
    Core f <*> o = fmap f o
    Layer f0 <*> o = Layer (f0 <*> o)

instance Monad Onion where
    Core x >>= f = f x
    Layer o >>= f = Layer( o >>= f)


----- Exercises
-- 1 

isolate :: Eq a => a -> [a] -> ([a], [a])
isolate _ [] = ([], [])
isolate x (y:ys)
    | x == y    = let (eq, neq) = isolate x ys
                  in (y:eq, neq)
    | otherwise = let (eq, neq) = isolate x ys
                  in (eq, y:neq)

frequencies :: String -> [(Char, Int)]
frequencies [] = []
frequencies (x:xs) =
    let (eq, rest) = isolate x (x:xs)
        count = length eq
    in (x, count) : frequencies rest

    
