-- ================================== 2013 ===================================

-- 2. a)
data PriorityQueue a = PriorityQueue [(a, Int)] | Empty
     deriving Show

-- 2. b)
enq :: a -> Int -> PriorityQueue a -> PriorityQueue a
enq a p Empty = PriorityQueue [(a, p)]
enq a p (PriorityQueue q) = PriorityQueue $ enq' a p q []
    where enq' a p [] nq = nq
          enq' a p ((q, qp):qs) nq | p == qp = nq ++ (q, qp):(a, p):qs
                                   | p < qp = nq ++ (a, p):(q, qp):qs
                                   | otherwise = enq' a p qs $ nq ++ [(q, qp)]

-- 2. c)
deq :: PriorityQueue a -> (a, PriorityQueue a)
deq Empty = error "Queue is empty"
deq (PriorityQueue ((q, _):qs)) = (q, PriorityQueue qs)

-- 2. e)
elts :: PriorityQueue a -> [a]
elts Empty = error "Queue is empty"
elts (PriorityQueue qs) = [e | (e, _) <- qs]

-- 3. b)
sum' l = foldl (+) 0 l

-- 3. c) i.
data BinTree a = BinTree a (BinTree a) (BinTree a) | EmptyTree

foldt :: (a -> a -> a) -> a -> BinTree a -> a
foldt f x EmptyTree = x
foldt f x (BinTree a l r) = f (f x a) (f (foldt f x l) (foldt f x r))

-- 3. c) ii.
sumTree t = foldt (+) 0 t