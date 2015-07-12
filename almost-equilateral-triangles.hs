import Control.Monad

isqrt = round . sqrt . fromInteger
square n = isqrt n ^ 2 == n

partial p f x = if p x then [f x] else []

sq = partial square isqrt

dv a b = if mod a b == 0 then [div a b] else []

sidelength = (`dv` 3) <=< (\x -> [x+1,x-1]) <=< sq . (\x -> 3*x^2 + 4)

sidelengths = takeWhile (\c -> 3*c-1 <= 10^9) $ concatMap sidelength [2,4..1732050808]

test1 c = square d && mod ((c+1) * isqrt d) 4 == 0
  where d = (3*c+1)*(c-1)

perim1 c = 3*c+1

test2 c = square d && mod ((c-1) * isqrt d) 4 == 0
  where d = (3*c-1)*(c+1)

perim2 c = 3*c-1

res = [perim c | c <- sidelengths, (test,perim) <- [(test1,perim1),(test2,perim2)], perim c <= 10^9, test c]
