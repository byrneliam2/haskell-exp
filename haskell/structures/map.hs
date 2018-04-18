-- Implementations of data structures

module HaskellExpMap where

-- ================================================     MAP    ================================================

type Map a b = [(a, b)]

emptyMap :: Map a b
emptyMap = []

hasKey :: Eq a => a -> Map a b -> Bool
hasKey x [] = False
hasKey x ((k, v):kvs) | k == x = True
                      | otherwise = hasKey x kvs

setVal :: Ord a => a -> b -> Map a b -> Map a b
setVal x y [] = [(x, y)]
setVal x y ((k, v):kvs) | x == k = (k, y):kvs
                        | x < k = (x, y):(k, v):kvs
                        | otherwise = (k, v):(setVal x y kvs)

getVal :: Eq a => a -> Map a b -> b
getVal x [] = error "error - key not present"
getVal x ((k, v):kvs) | x == k = v
                      | otherwise = getVal x kvs

delKey :: Eq a => a -> Map a b -> Map a b
delKey x [] = error "error - key not present"
delKey x ((k, v):kvs) | x == k = kvs
                      | otherwise = (k, v):(delKey x kvs)