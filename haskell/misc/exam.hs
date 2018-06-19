test :: Eq a => [a] -> Bool
test [_] = False
test (x:xs) = elem x xs