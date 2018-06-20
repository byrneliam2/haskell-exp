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
type Var = Char
type Store = [(Var, Bool)]

data Op = And | Or | Not | Implies
data BExp = Const Bool | Var Var | Op Op [BExp]

-- 3. a)
checkExp :: BExp -> Bool
checkExp (Const _) = True
checkExp (Var _) = True
checkExp (Op And l) = length l >= 2 && all (\x -> checkExp x) l
checkExp (Op Or l) = length l >= 2 && all (\x -> checkExp x) l
checkExp (Op Not [e]) = checkExp e
checkExp (Op Implies [e1, e2]) = checkExp e1 && checkExp e2
checkExp _ = error "Error!"

-- 3. b)
eval :: Store -> BExp -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Op op exps) = apply op $ map (\x -> eval s x) exps

apply :: Op -> [Bool] -> Bool
apply Not [v] = not v
apply Implies [v1, v2] = (not v1) || v2
apply And bs = and bs
apply Or bs = or bs

find :: Var -> Store -> Bool
find x s = head [v | (u, v) <- s, u == x]

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
data Tree a = Node2 a [Tree a] | Leaf a

findLeaves :: Tree a -> Int
findLeaves (Leaf _) = 1
findLeaves (Node2 _ t) = sum $ map findLeaves t

-- 2. b)
data Tree2 a c = Node3 a Int [Tree2 a Int] | Leaf2 a Int

-- 2. c)
leastCost :: Tree2 a c -> ([a], Int)
leastCost t = leastCost' t []

leastCost' :: Tree2 a c -> [a] -> ([a], Int)
leastCost' (Leaf2 a c) p = (p ++ [a], c)
leastCost' (Node3 a c t) p = getLeast $ map (\x -> leastCost' x (p ++ [a])) t

getLeast (p:ps) = getLeast' ps p
getLeast' [] p = p
getLeast' ((p, c):ps) (px, cx) | c < cx = getLeast' ps (p, c)
                               | otherwise = getLeast' ps (px, cx)

-- ================================== 2015 ===================================

-- 1. b) ii.
compress :: Eq a => [a] -> [a]
compress xs = compress' xs []
compress' [] l = l
compress' [x] l = l ++ [x] 
compress' (x:xs) l | x == y = compress' ys $ l ++ [y]
                   | otherwise = compress' xs $ l ++ [x]
                     where (y:ys) = xs