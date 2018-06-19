-- ================================== 2017 ===================================

-- 1. a)
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) | x == y = True
               | otherwise = elem' x ys

-- 1. b)
test :: Eq a => [a] -> Bool
test [_] = False
test (x:xs) = elem x xs

-- 1. c)
replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace o n l = map (\x -> if x == o then n else x) l