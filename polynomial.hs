data Polynomial a = Polynomial [a] deriving (Eq,Show)

zipWithDefault d f (x:xs) (y:ys) = f x y : zipWithDefault d f xs ys
zipWithDefault d f xs [] = zipWith f xs (repeat d)
zipWithDefault d f [] ys = zipWith f (repeat d) ys

instance Num a => Num (Polynomial a) where
  Polynomial as + Polynomial bs = Polynomial $ zipWithDefault 0 (+) as bs
  Polynomial (a:as) * Polynomial bs = Polynomial (map (*a) bs) + p
    where Polynomial cs = Polynomial as * Polynomial bs
          p = Polynomial (0 : cs)
  Polynomial [] * _ = Polynomial []
  negate (Polynomial as) = Polynomial (map negate as)
  signum (Polynomial as) = Polynomial [signum (last as)]
  abs p = p * signum p
  fromInteger = constant . fromInteger

x :: Num a => Polynomial a
x = Polynomial [0,1]

constant n = Polynomial [n]

integrate (Polynomial as) = Polynomial (0 : zipWith (/) as [1..])

evaluate x (Polynomial as) = sum . zipWith (*) as $ zipWith (^) (repeat x) [0..]

int a b p = evaluate b (integrate p) - evaluate a (integrate p)
