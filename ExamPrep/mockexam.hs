import Control.Applicative.Lift (Lift(Pure))
-- Problem 1

remove :: (Ord t, Num t) => [a] -> t -> [a]
remove [] k = []
remove (x:xs) k | k <= 1 = xs
remove (x:xs) k = x : (remove xs (k-1))


removals xs = [ remove xs i | i <- [1..length xs]]


-- Problem 2

data Person = Famous String | Normal String

data Unit = Single Person | Couple (Person,Person)

data Dynasty = A Unit [Dynasty]

-- Problem 3

dwindle :: [a] -> [[a]]
dwindle [] = [[]]
dwindle (x:xs) = [x:xs] ++ dwindle xs

dwindle' :: [a] -> [[a]]
dwindle' xs = [drop n xs | n <- [0..length xs]]

dwindle'' :: [a] -> [[a]]
dwindle'' xs = reverse (foldr (\x (h:t) -> (tail h):(h:t)) [xs] xs)

-- Problem 4

data Status a = Fresh a | Used a deriving Show

instance Functor Status where
    fmap f (Fresh x) = Fresh (f x)
    fmap f (Used x) = Used (f x)

instance Applicative Status where
    pure x = Fresh x
    (Fresh f) <*> (Fresh x) = Fresh (f x)
    (Used f) <*> (Fresh x) = Used (f x)
    (Used f) <*> (Used x) = Used (f x)
    (Fresh f) <*> (Used x) = Used (f x)

instance Monad Status where
    return = pure
    (Fresh x) >>= f = f x
    (Used x) >>= f = f x


minimise xz yz = do
    x <- xz
    y <- yz
    return (min x y)


-- Problem 5

funcA :: Num a => Bool -> a -> (a, a)
funcA f a | f == True = (a+2, a)

funcB :: a1 -> a2 -> (a2, a2) -> (a1, [a2])
funcB a xs (x,y) = (a,[y,x,xs])

funcC :: Maybe (Integer -> Integer)
funcC = Just (\x -> x+1)

funcD :: p1 -> p2 -> [a]
funcD _ _ = []

funcE :: Bool -> String
funcE True = "dingo"

-- Problem 6

squares = make 1 squares
    where
        make n squares = n*n : make (n+1) squares

squares' = map (\n -> n*n) [1..]