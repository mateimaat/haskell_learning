import Distribution.Simple.Program.HcPkg (HcPkgInfo(noVerboseFlag))
-- Problem 1


funcA :: a -> b -> [(a, b)] -> [(a, b)]
funcA a b xs = (a,b) : xs

funcB :: Num a => a -> a -> (a, [a])
funcB a b = (a,[b+a])

funcC :: (Ord a, Num a) => ([Char], a -> Bool)
funcC = (['c'], \a -> a+1 > 4)

funcD :: [[Char] -> [Char]]
funcD = [(reverse)]

-- Problem 2

data SWComponent = Leaf String Int | Node String Int [SWComponent] deriving Show

c1 = Node "Main" 20 [Node "Bingo" 12 [Leaf "Plip" 5, Leaf "Plop" 5, Leaf "Mango" 2], Leaf "Dingo" 8]

rootSize :: SWComponent -> Int
rootSize (Leaf _ size)   = size
rootSize (Node _ size _) = size

allvalid (Leaf _ size) = size > 0
allvalid (Node _ size subs) = 
    size > 0 && size == sum (map rootSize subs) && all allvalid subs


-- Problem 3

data Vote = Yes | No | Abstain deriving (Ord, Eq)

succes votes = measurey votes > measuren votes
    where
        measurey [] = 0
        measurey (Yes:xs) = 1 + measurey xs
        measurey (_:xs) = measurey xs
        measuren [] = 0
        measuren (No:xs) = 1 + measuren xs
        measuren (_:xs) = measuren xs

votes = [Yes, Yes, No, Abstain]

success' votes = length votesyes > length votesno
    where
        votesyes = [x | x <- votes, x == Yes]
        votesno = [x | x <- votes, x == No]


success'' votes = yesCount > noCount
  where
    (yesCount, noCount) = foldr count (0,0) votes
    count Yes (votesyes, votesno) = (votesyes+1, votesno)
    count No (votesyes, votesno) = (votesyes, votesno+1)
    count Abstain (votesyes, votesno) = (votesyes, votesno)


-- Problem 4

data Err a = Result a | Wrong Float deriving Show

safelog :: Float -> Err Float
safelog x | x > 0 = Result (log x)
    | otherwise = Wrong x

instance Functor Err where
    fmap f (Result x) = Result (f x)
    fmap f (Wrong x) = Wrong x

instance Applicative Err where
    pure = Result
    (Result f) <*> (Result x) = Result (f x)
    _ <*> (Wrong x) = Wrong x

instance Monad Err where
    return = pure
    (>>=) (Result x) g = g x
    (>>=) (Wrong x) g = Wrong x

safesum :: Float -> Float -> Err Float
safesum x y = do
    xs <- safelog x
    ys <- safelog y
    return (xs + ys)


-- Problem 5

odds = make 1 odds
    where
        make n odds = n : make (n+2) odds


tup = map (\n -> (n, 1/n)) odds