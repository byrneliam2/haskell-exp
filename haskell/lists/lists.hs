-- Experimenting with list functions
list1 = head [1, 2, 3] == 1
list2 = tail [1, 2, 3] == [2, 3]

-- Trying out head:tail pattern
list3 (x:xs) = x
list4 x xs = (x:xs)

-- Standard list functions
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty (_:_) = False

-- More standard functions
mysum [] = 0
mysum (x:xs) = x + mysum xs

-- Summing forwards using accumulator functions
mysum2 l = mysum2' l 0
mysum2' [] s = s
mysum2' (x:xs) s = mysum2' xs (x+s)

-- More accumulator examples
mylen l = mylen' l 0
mylen' [] l = l
mylen' (x:xs) l = mylen' xs l+1

-- Using local definitions
lsum1 l = let lsum1' [] s = s
              lsum1' (x:xs) s = lsum1' xs (x+s)
          in  lsum1' l 0
lsum2 l = lsum2' l 0
          where lsum2' [] s = s
                lsum2' (x:xs) s = lsum2' xs (x+s)

-- More interesting functions
union :: Ord a => [a] -> [a] -> [a]
union [] x = x
union x [] = x
union (x:xs) (y:ys) | x == y = x:(union xs ys)
                    | x < y = x:(union xs (y:ys))
                    | x > y = y:(union (x:xs) ys)

-- More interesting functions, part 2
allPositions :: Eq a => a -> [a] -> [Int]
allPositions n xs = allPositions' n xs 0 []
allPositions' n [] i is = reverse is
allPositions' n (x:xs) i is | n == x = allPositions' n xs (i+1) (i:is)
                            | otherwise = allPositions' n xs (i+1) is

-- List comprehensions
listc1 = [x*x | x <- [1..25], x `mod` 3 == 0]
listc2 = [(x, y) | x <- [1..3], y <- [1..3]]