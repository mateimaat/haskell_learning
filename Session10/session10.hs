-- Abstracting programming patterns

inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2 : sqr ns

map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

inc' :: [Integer] -> [Integer]
inc' = map (+1)
sqr' :: [Integer] -> [Integer]
sqr' = map (^2)

-- Generalising further

--class Functor f where
--    fmap :: (a->b) -> f a -> f b

{-- The list functor

instance Functor [] where
    -- fmap :: (a->b) -> [a] -> [b]
    fmap = map

-} --  The maybe functor

data Maybe' a = Nothing' | Just' a
instance Functor Maybe' where
    -- fmap :: (a->b) -> Maybe a -> Maybe b
    fmap g Nothing' = Nothing'
    fmap g (Just' x) = Just' (g x)

-- The tree functor

{- data Tree a = Leaf a | Node (Tree a) (Tree a)
t :: Tree int 
t = Node (Leaf 1) (Leaf 2) -}

{- instance Functor Tree where
    -- fmap :: (a->b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node L R) = Node (fmap g L) (fmap g R)

---}

-- inc'' :: Functor f => f int -> f int 
-- inc'' = fmap (+1)


-- Preparation Sheet
-- 1

data Onion a = Core a | Layer (Onion a)
  deriving (Show, Eq)

instance Functor Onion where
    -- fmap :: (a -> b) -> Onion a -> Onion b
    fmap f (Core x)    = Core (f x)
    fmap f (Layer o) = Layer (fmap f o)



instance Applicative Onion where
    pure = Core
    (Core g) <*> l = fmap g l
    (Layer g) <*> l = Layer (g <*> l)

onion = Layer (Layer (Layer (Layer(Core "bingo"))))


-- ex 1

data UTree a = Node a [UTree a] deriving Show

instance Functor UTree where
    fmap g (Node a xs) = Node (g a) [fmap g x | x <- xs]
    

myTree = Node "bingo" [ Node "logo" [], Node "dingo" [ Node "pop" [], Node "quango" [] ]]
myTree2 = Node 2 [ Node 3 [], Node 4 [Node 3 []]]


-- ex 4
{- instance Applicative [] where
    pure x = [x]
    xs <*> ys <*> zs = [x * y * z | x <- xs, y <- ys, z <- zs] -}

