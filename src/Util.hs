module Util where
    
count :: (a -> Bool) -> [a] -> Int
count pred list = sum [ 1 | x <- list, pred x ]

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

changeNth :: Int->a->[a]->[a]
changeNth n e (x:xs)
     | n == 0 = e:xs
     | otherwise = x:changeNth (n-1) e xs