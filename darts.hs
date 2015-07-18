data Score = S Int | D Int | T Int deriving (Eq,Show,Ord)

score (S p) = p
score (D p) = 2 * p
score (T p) = 3 * p

double (D p) = True
double _     = False

scores = S 25 : D 25 : [m p | m <- [S,D,T], p <- [1..20]]

sorted (x:y:ys) = x <= y && sorted (y:ys)
sorted _        = True

listsOf n e | n > 0  = [x:xs | x <- e, xs <- listsOf (n-1) e]
            | n == 0 = [[]]

checkouts = do
  darts <- concatMap (flip listsOf scores) [1..3]
  True <- [double (head darts)]
  True <- [sorted (tail darts)]
  True <- [sum (map score darts) < 100]
  return darts
