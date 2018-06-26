-- ================================== 2011 ===================================

-- 2. a)
type Bag a = [a]

-- 2. b)
add :: Eq a => Bag a -> a -> Bag a
add b x = x:b

-- 2. c)
del :: Eq a => Bag a -> a -> Bag a
del b x = del' b x []
    where del' [] _ n = n
          del' (b:bs) x n | b == x = n ++ bs
                          | otherwise = del' bs x $ n ++ [b]

-- 2. d)
union :: Bag a -> Bag a -> Bag a
union b1 b2 = b1 ++ b2

-- 2. e)
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True
subbag _ [] = False
subbag (b1:bs) b2 | b1 `elem` b2 = subbag bs $ del b2 b1
                  | otherwise = False

-- 3. a)
adjacent :: Int -> Int -> Int -> Int -> [(Int, Int)]
adjacent x y w h | w < 1 || h < 1 = error "negative dimensions"
                 | x < 1 || y < 1 || x > w || y > h = error "out of bounds"
                 | otherwise = filter (\(ax, ay) -> ax >= 1 && ay >= 1 && ax <= w && ay <= h)
                   [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

-- 3. b)
data Tree a = Node a [Tree a] | Leaf a
leaves :: Tree a -> Int
leaves (Leaf _) = 1
leaves (Node _ t) = foldl (+) 0 $ map leaves t

-- 3. d)
sumEvenSquares :: [Int] -> Int
sumEvenSquares l = foldl (+) 0 $ map (\x -> x * x) $ filter even l