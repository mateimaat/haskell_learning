--3.
data Tree a = Leaf a | Node (Tree a) (Tree a)

minmax :: Ord a => Tree a-> Maybe (a,a)
minmax (Leaf x)= Just(x,x)
minmax (Node l r)= do 
            (minL,maxL)<- minmax l
            (minR,maxR)<- minmax r
            if maxR >= minL 
            then return (min minL minR,max maxL maxR)
            else Nothing

minorder :: Ord a => Tree a -> Maybe a
minorder t = do
    (minimum,_)<-minmax t
    return (minimum)
t= Node (Leaf "aha") (Leaf "plip") --Just "aha"
t'=Node (Leaf 5) (Node (Leaf 2)(Leaf 3)) --Nothing