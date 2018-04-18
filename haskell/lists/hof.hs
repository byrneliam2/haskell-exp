-- Higher order functions and lambdas

-- Get all even numbers from a list of ints
evens :: [Int] -> [Int]
evens l = filter (\x -> x `mod` 2 == 0) l

-- Using maps to increment numbers in a list
incAll :: [Int] -> [Int]
incAll l = map (\x -> x + 1) l
incAllBy :: [Int] -> Int -> [Int]
incAllBy l n = map (\x -> x + n) l

-- Filters with sections
allPositives :: [Int] -> [Int]
allPositives l = filter (>= 0) l

-- Convert list of digits to number
convert :: [Int] -> Int
convert l = foldl (\x y -> (10 * x) + y) 0 l

-- Insertion sort with folds
isort :: (Ord a) => a -> [a] -> [a]
isort x []     = [x]
isort x [y]    | x <= y = x:[y]
               | otherwise = y:[x]
isort x (y:ys) | x <= y = x:y:ys
               | otherwise = y:(isort x ys)
sort :: (Ord a) => [a] -> [a]
sort l = foldr (isort) [] l

-- All positions of element in a list (much neater compared to first version in lists.hs!)
allPositions :: Eq a => a -> [a] -> [Int]
allPositions x l = filter (/= -1) (map (\(i, y) -> if x == y then i else -1) (zip [0..] l))