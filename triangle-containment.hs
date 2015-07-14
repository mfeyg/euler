slope_intercept (x1,y1) (x2,y2) = (m, y2 - m*x2)
  where m = (y2 - y1) / (x2 - x1)

origin_side (x1,y1) (x2,y2) (x3,y3)
  | y2 == y3 = abs y1 <= abs y2
  | otherwise = signum b * y1 <= signum b * (m * x1 + b)
  where (m,b) = slope_intercept (x2,y2) (x3,y3)

triangle p1 p2 p3 = and $ do
  (a,b,c) <- [(p1,p2,p3),(p2,p1,p3),(p3,p1,p2)]
  return $ origin_side a b c

parsetriangle line = triangle (x1,y1) (x2,y2) (x3,y3)
  where (x1,y1,x2,y2,x3,y3) = read $ "(" ++ line ++ ")"

main = do
  file <- readFile "p102_triangles.txt"
  print . length . filter parsetriangle $ lines file
