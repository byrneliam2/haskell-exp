-- ================================== 2016 ===================================

-- 1. a) and b)
count :: Eq a => a -> [a] -> Int
count _ [] = 0
count n (x:xs) | n == x = 1 + count n xs
               | otherwise = count n xs

-- 1. c)
count2 :: Eq a => a -> [a] -> Int
count2 x l = length [n | n <- l, n == x]

-- 1. d)
count3 :: Eq a => a -> [a] -> Int
count3 x l = length [i | (i, n) <- zip [0..] l, n == x]

-- 1. f)
count4 :: Eq a => a -> [a] -> Int
count4 x l = foldr (\x y -> if x then y + 1 else y) 0 $ map (== x) l

-- 2. a)
data Tree a = Node a [Tree a] | Leaf a

findLeaves :: Tree a -> Int
findLeaves (Leaf _) = 1
findLeaves (Node _ t) = sum $ map findLeaves t

-- 2. b)
data Tree2 a c = Node2 a Int [Tree2 a Int] | Leaf2 a Int

-- 2. c)
leastCost :: Tree2 a c -> ([a], Int)
leastCost t = leastCost' t []

leastCost' :: Tree2 a c -> [a] -> ([a], Int)
leastCost' (Leaf2 a c) p = (p ++ [a], c)
leastCost' (Node2 a c t) p = getLeast $ map (\x -> leastCost' x (p ++ [a])) t

getLeast (p:ps) = getLeast' ps p
getLeast' [] p = p
getLeast' ((p, c):ps) (px, cx) | c < cx = getLeast' ps (p, c)
                               | otherwise = getLeast' ps (px, cx)