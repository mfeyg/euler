{-# LANGUAGE FlexibleContexts #-}

import Math.Polynomial
import Math.Polynomial.Bernoulli

-- sum [n^p, n <- [0..x]]
pps p = composePoly b (poly LE [1,1]) `addPoly` constPoly (-(evalPoly b 0))
  where b = bernoulliPoly !! (p+1) `multPoly` constPoly (fromIntegral $ product [1..p])

polySum p = foldr addPoly zero
                  [pps i `multPoly` constPoly a | (a,i) <- zip (polyCoeffs LE p) [0..]]

to n p = constPoly (evalPoly p n) `addPoly` negatePoly (composePoly p (poly LE [-1,1]))

fpow 0 f = id
fpow n f = fpow (n-1) f . f

incr n =  flip evalPoly 1 . to n . polySum
                          . fpow 8 (to (n+1) . polySum)
                          $ constPoly 1

decr n =  flip evalPoly 1 . to n . polySum
                          . fpow 9 (to (n+1) . polySum)
                          $ constPoly 1

nonbouncy :: Rational -> Rational
nonbouncy digits = incr digits + decr digits - 10 * digits
