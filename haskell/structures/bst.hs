-- Implementations of data structures

module HaskellExpBST where

-- ================================================     BST    ================================================

data BinTree a = Empty | Node a (BinTree a) (BinTree a)
                 deriving (Show, Eq, Ord)

empty :: BinTree a
empty = Empty

insert :: Ord a => a -> BinTree a -> BinTree a
insert x Empty = Node x Empty Empty
insert x (Node n l r) | x == n = Node n l r
         | x < n && l == Empty = Node n (Node x Empty Empty) r
         | x > n && r == Empty = Node n l (Node x Empty Empty)
         | x < n = Node n (insert x l) r
         | x > n = Node n l (insert x r)

has :: Ord a => a -> BinTree a -> Bool
has x Empty = False
has x (Node n l r) | x == n = True
      | (x < n && l == Empty) || (x > n && r == Empty) = False
      | x < n = has x l
      | x > n = has x r

delete :: Ord a => a -> BinTree a -> BinTree a
delete x Empty = Empty
delete x (Node n l r) | x == n && l == Empty && r == Empty = Empty
         | x == n && l /= Empty && r == Empty = Node ((\(Node n _ _) -> n) l) ((\(Node _ l _) -> l) l) r
         | x == n && l == Empty && r /= Empty = Node ((\(Node n _ _) -> n) r) l ((\(Node _ _ r) -> r) r)
         | x == n && l /= Empty && r /= Empty = Node (getlmost r) l (delete (getlmost r) r)
         | x < n = Node n (delete x l) r
         | x > n = Node n l (delete x r)
           where getlmost (Node n l r) = if l == Empty then n else getlmost l

flatten :: Eq a => BinTree a -> [a]
flatten Empty = []
flatten (Node n l r) = flatten' (Node n l r) []
flatten' (Node n l r) xs | l == Empty && r == Empty = xs ++ [n]
            | l /= Empty && r/= Empty = flatten' l xs ++ [n] ++ flatten' r xs
            | l /= Empty = xs ++ flatten' l xs ++ [n]
            | r /= Empty = xs ++ [n] ++ flatten' r xs

equals :: Ord a => BinTree a -> BinTree a -> Bool
equals Empty Empty = True
equals _ Empty = False
equals Empty _ = False
equals a b = all (\n -> has n b) (flatten a)

treesort :: Ord a => [a] -> [a]
treesort [] = []
treesort l = flatten (foldr (\x n -> insert x n) Empty l)