encode :: String -> [(Integer, Char)]
encode (x:xs) = compact (convert [x]) (convert xs)

convert :: [Char] -> [(Integer, Char)]
convert [] = []
convert (x:xs) = [(1, x)] ++ (convert xs)

compact :: [(Integer, Char)] -> [(Integer, Char)] -> [(Integer, Char)]
compact x [] = x
compact x (y:ys)
    | snd(last x) == snd y = compact (incrementLastTuple x) ys
    | snd(last x) /= snd y = compact (x ++ [y]) ys

incrementLastTuple :: [(Integer, Char)] -> [(Integer, Char)]
incrementLastTuple a = reverse $ (fst (last a) + 1, snd (last a)):(tail (reverse a))

-- ============================================================================

decode :: [(Integer, Char)] -> [Char]
decode xs = extract xs []

extract :: [(Integer, Char)] -> [Char] -> [Char]
extract [] cs = cs
extract (x:xs) cs = extract xs (cs ++ formString x [])

formString :: (Integer, Char) -> [Char] -> [Char]
formString (0, _) cs = cs
formString (x, y) cs = formString (x - 1, y) (cs ++ [y])