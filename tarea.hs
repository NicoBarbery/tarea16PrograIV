--a)count' n xs = length (filter (==n) (map length ×s) )
count' n = length . filter (==n) . map length
--b) any p ×s = or (map p xs)
any p xs = or . map p
--c) last xs = head (reverse xs)
last xs = head (reverse ×s)
----------------------------------------------------------
mid [] = error "Error lista vacia"
mid [x] = error "Lista Unitaria"
mid xs = mid' [] xs
         where mid' acc [x,y] = acc
               mid' acc (x:y:xs) = mid' (acc ++ [y]) (y:xs)
------------------------------------------------------------

mid'' = tail . init

------------------------------------------------------------

memberNum :: Int -> [Int] -> Int
memberNum n [1 = 0
memberNum n (xixs) | n =a x
1 + memberNum n xs
otherwise = memberNum n xs

memberNum' n xs = length filter (n==) xs

-------------------------------------------------------------

unique :: [Int] -> (Int]
unique ls = filter p xs
             where p x = memberNum x xs == 1
1
