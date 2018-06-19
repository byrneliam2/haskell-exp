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

-- 2.
data BinTree a = Node a (BinTree a) (BinTree a) | Empty

-- 2. b)
height :: BinTree a -> Int
height Empty = 0
height (Node a l r) = 1 + max (height l) (height r)

height_t1 = height (Node 8 (Node 3 (Node 1 Empty Empty) (Node 6 (Node 4 Empty 
            (Node 5 Empty Empty)) (Node 7 Empty Empty))) (Node 10 (Node 9 Empty Empty) 
            (Node 14 (Node 13 Empty Empty) Empty)))

-- 2. c)
data BinTreeC a c = NodeC a Int (BinTreeC a Int) (BinTreeC a Int) | EmptyC

-- 2. d)
sumCosts :: BinTreeC a c -> Int
sumCosts EmptyC = 0
sumCosts (NodeC n c l r) = c + (sumCosts l + sumCosts r)

sumCosts_t1 = sumCosts $ NodeC 'a' 0 (NodeC 'a' 1 EmptyC EmptyC) (NodeC 'a' 4 EmptyC EmptyC)

-- 3.
data Op = And | Or | Not | Implies
data BExp = Var Char | Op Op [BExp]

-- 3. a)
checkExp :: BExp -> Bool
checkExp (Var _) = True
checkExp (Op And l) = length l >= 2 && all (\x -> checkExp x) l
checkExp (Op Or l) = length l >= 2 && all (\x -> checkExp x) l
checkExp (Op Not [e]) = checkExp e
checkExp (Op Implies [e1, e2]) = checkExp e1 && checkExp e2
checkExp _ = error "Error!"