import Data.Array
import Data.Maybe
import Data.List (minimumBy)
import Data.Function

type Node = Int
type Weight = Int
type Network = Array (Node,Node) (Maybe Weight)

addEdge :: Node -> Node -> Weight -> Network -> Network
addEdge x y w = (//[((x,y),Just w), ((y,x),Just w)])

empty :: Network -> Network
empty = flip listArray (repeat Nothing) . bounds

minEdge :: [Node] -> [Node] -> Network -> (Node,Node)
minEdge from to network = minimumBy edgeWeight [(x,y) | x <- from, y <- to]
  where edgeWeight e1 e2 = compare' (network!e1) (network!e2)
        compare' Nothing Nothing = EQ
        compare' Nothing _ = GT
        compare' _ Nothing = LT
        compare' x y = compare x y

prim :: Network -> Network
prim network = go [1] [2..n] (empty network)
  where go _ [] g = g
        go c u g = let (x,y) = minEdge c u network in
                      go (y:c) (filter (/=y) u)
                         (addEdge x y (fromJust $ network!(x,y)) g)
        n = fst . snd . bounds $ network

sumWeights :: Network -> Weight
sumWeights = (`div` 2) . sum . catMaybes . elems

parseLine :: String -> [Maybe Int]
parseLine "" = []
parseLine line | head line == '-' = Nothing : parseLine (drop 2 line)
               | otherwise =
                 let [(n,r)] = reads line in Just n : parseLine (drop 1 r)

parseFile :: String -> Network
parseFile file = listArray ((1,1),(n,n)) $ concatMap parseLine (lines file)
  where n = length (lines file)

main = do
  file <- readFile "p107_network.txt"
  let network = parseFile file
  print (sumWeights network - sumWeights (prim network))
