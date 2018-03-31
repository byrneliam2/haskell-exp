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