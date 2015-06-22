import Data.List
import Control.Monad (guard)

up3 f a b c x = f (a x) (b x) (c x)

if' p a b = if p then a else b

if2  = up3 if'

tr a b = map $ if2 (==a) (const b) id

squares = map (tr '9' '6' . reverse . take 2 . reverse . ('0':) . show . square)
              [1..9]
  where square n = n^2

opposites d = filter (/= d) . concat . filter (d `elem`) $ squares

valid = filter ((\s -> null . intersect s . concatMap opposites $ s)
                . (\\"6") . tr '9' '6')
      . filter ((==4) . length)

solns = do
  a <- valid . subsequences $ ['0'..'9']
  b <- valid . subsequences $ ['0'..'9'] \\ (a \\ "769")
  guard $ a < b
  guard . not . and $ [x `elem` y | x <- "69" , y <- [a,b]]
  return (a,b)

main = print . length $ solns
