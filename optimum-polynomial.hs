import Data.Ratio

u n = sum [(-n)^i | i <- [0..10]]

approx k f = lagrange [(x, f x) | x <- [1..k]]

-- Each term of a sequence paired with the rest of the sequence
-- parts [x i | i <- [1..k]] = [(x i, [x j | j <- [1..k], j /= i]) | i <- [1..k]]
parts = go id
  where go xs (y:ys) = (y, xs ys) : go (xs . (y:)) ys
        go _ [] = []

-- Define a Langrange polynomial `f` given target points [(xi,yi)] s.t. f xi = yi
lagrange pts x = sum $ map (`term` x) (parts pts)

term ((xj,yj), rest) x = (yj%1) * product [(x-xm)%(xj-xm) | (xm,_) <- rest]

soln = sum [approx i u (i+1) | i <- [1..10]]
