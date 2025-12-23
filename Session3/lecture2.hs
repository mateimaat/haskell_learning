inm :: Int -> ( Int -> ( Int -> Int))
inm x y z = x * y * z

quango :: a -> [ a ]
quango n = [n]

tango :: Num p1 => ( a , b ) -> p2 -> p1
tango (a,b) p2 = 10


twice f x = f(f(x))
