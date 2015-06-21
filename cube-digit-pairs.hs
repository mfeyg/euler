import Data.List
import Control.Monad (guard)

squares = ["01","04","06","16","25","36","46","81"]

digits = "01234568"

opposites d = squares >>= (\ s -> if d `elem` s then s \\ [d] else [])

choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k

missed = do
  missing <- subsequences digits
  if '6' `elem` missing
    then guard $ length missing `elem` [2,3]
    else guard $ length missing `elem` [2,3,4]
  guard . null $ missing `intersect` concatMap opposites missing
  return missing

soln = sum $ do
  missing <- missed
  return $ let m = length missing in
    if '6' `elem` missing then choose (9 - m) (5 - m) + 2 * choose (9 - m) (6 - m)
      else choose (10 - m) (6 - m) * case m of 2 -> 2
                                               3 -> 3
                                               4 -> 1

main = return ()
