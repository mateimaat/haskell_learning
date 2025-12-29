-- Problem 1

rotate (x:xs) = xs ++ [x]
-- function is parametric polymorphic

allrotates [] = []
allrotates xs = xs : take (length xs - 1) (allrotates (rotate xs))

allrotates' xs = map (\n -> iterate rotate xs !! n) [0.. length xs -1]

allrotates'' xs = take (length xs) (iterate rotate xs)

-- Problem 2

data PartTree a = Leaf a | Node (Maybe a) (PartTree a) (PartTree a) deriving Show

t1 = Node (Nothing) (Node (Just 12) (Leaf 17) (Leaf 484000)) (Leaf 1964)


isfull (Leaf _) = True
isfull (Node Nothing _ _) = False
isfull (Node (Just _) left right) = isfull left && isfull right


preorder (Leaf x) = return [x]
preorder (Node Nothing _ _) = Nothing
preorder (Node (Just x) left right) = do
    leftList  <- preorder left
    rightList <- preorder right
    return (x : leftList ++ rightList)


-- Problem 3

remove xs ys = [y | y <- ys, y `notElem` xs]


remove' _ [] = []
remove' xs (y:ys) 
    | y `elem` xs = remove xs ys
    | otherwise = y : remove xs ys


-- Problem 4

newtype WrapString a = WS (a,String) deriving Show

instance Functor WrapString where
fmap f (WS (x,s)) = WS (f x,s)

instance Applicative WrapString where
    pure x = WS (x, "")
    WS (f, s1) <*> WS (x, s2) = WS (f x, s1 ++ s2)


instance Monad WrapString where
    return = pure
    WS (x, s1) >>= f = 
        let WS (y, s2) = f x
        in WS (y, s1 ++ s2)


pairup ws1 ws2 = do
    x <- ws1
    y <- ws2
    return (x, y)



-- Problem 5

funcA :: (Ord b, Num b) => b -> b -> b -> (b, b)
funcA a b c
    | a > c && b > c = (a,a+b+c)
    | otherwise = (a, a+3)

-- adhoc polymorphic

funcB :: [(Integer, t -> Char)]
funcB = [(1, \n -> 'c')]


funcC :: (t1 -> Bool -> t2) -> t1 -> t2
funcC f x = f x True
-- parametric polymorphic



funcD a = [1..a]
-- adhoc polymorphic

-- Problem 6

naturals = 1 : next 1 naturals
    where
       next n naturals = n+1 : next (n+1) naturals


facs = map fact [0..]
    where
        fact 0 = 1
        fact n = n * fact (n-1)


facs' :: [Integer]
facs' = 1 : zipWith (*) facs' [1..]
