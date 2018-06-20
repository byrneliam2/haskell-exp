-- ================================== 2015 ===================================

-- 1. b) ii.
compress :: Eq a => [a] -> [a]
compress xs = compress' xs []
compress' [] l = l
compress' [x] l = l ++ [x] 
compress' (x:xs) l | x == y = compress' ys $ l ++ [y]
                   | otherwise = compress' xs $ l ++ [x]
                     where (y:ys) = xs

-- 1. c)
filter' :: (a -> Bool) -> [a] -> [a]
filter' f x = [y | y <- x, f y]