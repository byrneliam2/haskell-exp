-- ================================== 2010 ===================================

-- 5. b)
data Tree a = Node a [Tree a] | Leaf a

height :: Tree a -> Int
height t = height' t 0
    where height' (Leaf _) x = x
          height' (Node _ t) x = maximum $ map (\y -> height' y (x + 1)) t

-- 5. c) i.
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 f l = filter1' f l []
    where filter1' f [] l = l
          filter1' f (x:xs) l | f x = filter1' f xs $ l ++ [x]
                              | otherwise = filter1' f xs l

-- Alternatively,
filter1b :: (a -> Bool) -> [a] -> [a]
filter1b f [] = []
filter1b f (x:xs) | f x = x:(filter1b f xs)
                  | otherwise = filter1b f xs

-- 5. d)
filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f l = concat (map (\x -> if f x then [x] else []) l)