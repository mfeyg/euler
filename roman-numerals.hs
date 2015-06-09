import Data.Bimap hiding (map)

denoms = fromList [('I',1),
                   ('V',5),
                   ('X',10),
                   ('L',50),
                   ('C',100),
                   ('D',500),
                   ('M',1000)]

parseNumeral :: String -> Int
parseNumeral = go . map (denoms!)
  where go (n:m:ms) | n < m  = m - n + go ms
                    | n >= m = n + go (m:ms)
        go ms = sum ms

romanize n | 0 <= n && n < 4 = replicate n 1
           | n == 4 = [1,5]
           | 5 <= n && n < 9 = 5 : replicate (n-5) 1
           | n == 9 = [1,10]

showNumeral n = map (denoms!>) $
  replicate (n `div` 1000) 1000 ++
  concat [(m*) `map` romanize (digit m) | m <- [100,10,1]]
 where digit m = n `mod` (10*m) `div` m

main = do
  unopt <- readFile "p089_roman.txt"
  print $ sum [length n - length (showNumeral . parseNumeral $ n)
              | n <- lines unopt]
