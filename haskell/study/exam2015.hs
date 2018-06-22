-- ================================== 2015 ===================================

-- 1. a)
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):(zip' xs ys) 

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

-- 1. d)
samePos :: Eq a => [a] -> [a] -> [a]
samePos _ [] = []
samePos [] _ = []
samePos xs ys = [n1 | (m1, n1) <- zip [0..] xs, (m2, n2) <- zip [0..] ys, 
    (m1, n1) == (m2, n2)]

-- 2.
data BinTree a = Leaf a | Bin a (BinTree a) (BinTree a)
    deriving Show

-- 2. a)
fringe :: BinTree a -> [a]
fringe (Leaf a) = [a]
fringe (Bin a l r) = (fringe l) ++ (fringe r)

-- 2. c)
btMap :: (a -> a) -> BinTree a -> BinTree a
btMap f (Leaf a) = Leaf (f a)
btMap f (Bin a l r) = Bin (f a) (btMap f l) (btMap f r)