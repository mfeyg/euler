{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Data.Char
import Data.Array
import Control.Monad
import Data.List
import Data.Function

type Grid = Array (Int,Int) Int

parseGrid :: Stream s m Char => ParsecT s () m Grid
parseGrid = listArray ((1,1),(9,9)) . concat <$>
              count 9 (count 9 (digitToInt <$> digit) <* (void newline <|> eof))

parseSudoku :: Stream s m Char => ParsecT s () m [Grid]
parseSudoku = many (string "Grid " >> count 2 digit >> newline >> parseGrid)

withSudoku f = either (return . Left) (fmap Right) =<<
               fmap f
             . parse parseSudoku "p096_sudoku.txt"
           <$> readFile "p096_sudoku.txt"

showGrid g = sequence_
               [sequence_
                 [(putChar . intToDigit $ g!(x,y))
                  >> putChar ' '
                 | y <- [1..9]]
                 >> putChar '\n'
               | x <- [1..9]]

adjacentTo (x,y) = nub $
                   [(x,y') | y' <- [1..9]] ++
                   [(x',y) | x' <- [1..9]] ++
                   [(x',y') | x' <- [x_..x_+2], y' <- [y_..y_+2]]
      where x_ = 3 * div (x-1) 3 + 1
            y_ = 3 * div (y-1) 3 + 1

possibilities grid pos =
  if grid ! pos /= 0 then [grid ! pos]
  else [1..9] \\ map (grid!) (adjacentTo pos)

nextIx grid = minimumBy (compare `on` (length . possibilities grid))
            . filter (\ix -> grid ! ix == 0)
            $ range ((1,1),(9,9))

solve grid = if all (/=0) grid then return grid else do
  a <- possibilities grid (nextIx grid)
  solve (grid // [(nextIx grid, a)])

number grid = 100 * (grid!(1,1)) + 10 * (grid!(1,2)) + grid!(1,3)

main = withSudoku $
  print . sum . map number . concatMap solve
