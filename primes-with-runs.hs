import Data.Foldable (foldl')

twos n | n `mod` 2 == 0 = let (s,d) = twos (n `div` 2) in (s+1,d)
       | otherwise      = (0,n)

powmod _ 0 _ = 1
powmod x e m = (if odd e then x else 1) * powmod x (e `div` 2) m ^ 2 `mod` m

witness a n = powmod a d n /= 1 && and [powmod a (2^r*d) n /= n-1 | r <- [0..s-1]]
  where (s,d) = twos (n-1)

prime n = n > 1 && (not . any (`witness` n) . filter (/=n) $ [2,13,23,1662803])

-- n-digits numbers where m of the digits are d
digits m n d = map (foldl' (\a b -> 10*a + b) 0)
             . filter ((>0) . head) $ concatMap (make n) (changes (n - m))
  where make i (c:cs) | i == c = [x:xs | x <- [0..9], x /= d, xs <- make (i-1) cs]
                      | i > c  = map (d:) (make (i-1) (c:cs))
        make i [] = [replicate i d]
        changes 0 = [[]]
        changes m = [c:cs | cs <- changes (m-1), c <- [next cs..n]]
        next [] = 1
        next (c:cs) = c + 1

m n d = go n
  where go m | any prime (digits m n d) = m
             | otherwise = go (m-1)

s n d = sum . filter prime $ digits (m n d) n d

main = print $ sum [s 10 d | d <- [0..9]]
