midtover :: [a] -> ([a], [a])

midtover xs = (left , right)
           where 
            halfway = length xs `div` 2
            left = take halfway xs 
            right = drop halfway xs


        
iseven n = (n `mod` 2 == 0)
-- isodd n = not(iseven)

