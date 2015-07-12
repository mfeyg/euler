import Data.List
import Data.Function

vlen x y = x^2 + y^2
rt (x1,y1) (x2,y2) =
  let [la,lb,lc] = sort [vlen x1 y1, vlen x2 y2, vlen (x2-x1) (y2-y1)]
  in la > 0 && lc == la + lb

rts = flip div 2 . length $ do
  x1 <- [0..50]
  y1 <- [0..50]
  x2 <- [0..50]
  y2 <- [0..50]
  True <- return $ rt (x1,y1) (x2,y2)
  return [((x1,y1),(x2,y2))]
