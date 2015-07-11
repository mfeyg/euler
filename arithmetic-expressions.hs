import Data.List
import Data.Function (on)
import Data.Ratio

choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = [(x:r) | r <- choose (n-1) xs] ++ choose n xs

ops :: Rational -> Rational -> [Rational]
ops a b = [a+b,a-b,a*b,b-a] ++ if a /= 0 then [b/a] else [] ++ if b /= 0 then [a/b] else []

straightFrom a (x:xs) | a == x = straightFrom (a+1) xs
                      | otherwise = a-1

integral r = denominator r == 1

exprs ns = straightFrom 1 . nub . sort . filter integral . filter (>=1) $ do
  [x,y] <- choose 2 ns
  [z] <- choose 1 (ns \\ [x,y])
  w <- ns \\ [x,y,z]
  a <- ops x y
  b <- ops a z
  d <- ops b w
  e <- ops z w
  f <- ops a e
  [d,f]

solve = maximumBy (compare `on` fst) $ do
  a <- [1..9]
  b <- [a+1..9]
  c <- [b+1..9]
  d <- [c+1..9]
  return (exprs [a,b,c,d], (a,b,c,d))
