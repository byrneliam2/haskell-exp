-- Experimenting with list functions
list1 = head [1, 2, 3]
list2 = tail [1, 2, 3]

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