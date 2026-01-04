import Distribution.Simple (KnownExtension(TypeAbstractions))
import GHC.Internal.TH.Lib (noSourceStrictness)
-- Problem 1

funcA a b xs = (a,b) : xs
-- parametric polymorphic

funcB a b = (a+b, [a+1])
-- adhoc polymorphic

funcC :: (Char, Integer -> Bool)
funcC = ('c', \n -> n > 0)

funcD = (reverse)


-- Problem 2

data SWComponent = Leaf String Int | Node String Int [SWComponent] deriving Show

data SWComponent1 = Comp String Int [SWComponent1]

c1 = Node "Main" 20 [Node "Bingo" 12 [Leaf "Plip" 5, Leaf "Plop" 5, Leaf "Mango" 2], Leaf "Dingo" 8]

rootSize :: SWComponent -> Int
rootSize (Leaf _ size)   = size
rootSize (Node _ size _) = size

valid :: SWComponent -> Bool
valid (Leaf _ size) = size > 0
valid (Node _ size subs) =
    size > 0 && size == sum (map rootSize subs) && all valid subs


-- Problem 3

data Vote = Yes | No | Abstain deriving (Ord, Eq)

success votes = measureYes votes > measureNo votes 
    where
        measureYes [] = 0
        measureYes (Yes:xs) = 1 + measureYes xs
        measureYes (_:xs) = measureYes xs
        measureNo [] = 0
        measureNo (No:xs) = 1 + measureNo xs
        measureNo (_:xs) = measureNo xs

t1 = [Yes, Yes, Yes, Abstain]
t2 = [Yes, No, No, Abstain]

success' votes = yess > noss 
    where 
        yess = [Yes | Yes <- votes]
        noss = [No | No <- votes]


success'' votes = yesCount > noCount
  where
    (yesCount, noCount) = foldr count (0,0) votes
    count Yes (votesyes, votesno) = (votesyes+1, votesno)
    count No (votesyes, votesno) = (votesyes, votesno+1)
    count Abstain (votesyes, votesno) = (votesyes, votesno)


-- Problem 4

data Err a = Result a  | Wrong Float deriving Show

safelog x | x > 0 = Result (log x)
    | otherwise = Wrong x


instance Functor Err where
    fmap f (Result x) = Result (f x)
    fmap f (Wrong x) = Wrong x

instance Applicative Err where
    pure x = Result x
    (Result f) <*> (Result x) = Result (f x)
    _ <*> (Wrong x) = Wrong x  

instance Monad Err where
    return = pure
    (>>=) (Result x) g = g x
    (>>=) (Wrong x) g = Wrong x


safesum x y = do
    xs <- safelog x
    ys <- safelog y
    return (xs + ys)


-- Problem 5

odds = 1 : next 1 odds
    where 
        next n odds = (n+2) : next (n+2) odds

tup = map (\x -> (x, 1 / x)) odds

plop x = plop x
quango u v = u ++ (if (length u) > 3 then "ringo" else v)

-- Problem 6

ft [] _ = []
ft ((x, f):xs) v | x == v = if f == 1 then xs else (x, f - 1):xs 
                 | otherwise = (x, f):(ft xs v)

-- validfq :: Eq a, Eq b, Num b => [(a,b)] -> [a] -> Bool
validfq fs vs = (reduce fs vs) == [] 
  where 
    reduce fs [] = fs 
    reduce fs (x:vs) = ft (reduce fs vs) x 