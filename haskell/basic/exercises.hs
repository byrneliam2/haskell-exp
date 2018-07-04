-- Exercises as suggested in lectures

-- indexOf function, directly from slides
pos :: Eq a => a -> [a] -> Int
pos x l = pos' x 1 l
pos' :: Eq a => a -> Int -> [a] -> Int
pos' _ _ [] = 0
pos' x k (y : _) | x == y = k
pos' x k (_ : ys) = pos' x (k+1) ys

-- Fibonacci without accumulators
fibb x | x < 2 = 1
       | otherwise = fibb(x-1) + fibb(x-2)

-- Fibonacci with accumulators
fiba x = fiba' x 0
  where fiba' 2 f = 1
        fiba' x f = fiba' (x-1) + fiba' (x-2) 