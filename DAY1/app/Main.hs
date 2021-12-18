module Main where

import Control.Monad
import Data.Char
import Data.Text.Read

main = do
    contents <- getContents
    print (computeDeeper 0 (map read (lines contents)))
    print (computeDeeperSW 0 (map read (lines contents)))

-- count number of increases in depth in the input file
-- input is list of numbers

computeDeeper :: Int -> [Int] -> Int
computeDeeper a [x] = a
computeDeeper a (x:xs)
    |  head xs > x = computeDeeper (1 + a) xs
    | otherwise = computeDeeper a xs


-- count number of increases in depth in the input file using a sliding window of 3
-- input is list of numbers

computeDeeperSW :: Int -> [Int] -> Int
computeDeeperSW a [x] = a
computeDeeperSW a [x, y] = a
computeDeeperSW a [x, y, z] = a
computeDeeperSW a (x:xs)
    |  sum (take 3 xs) > sum (take 3 (x:xs)) = computeDeeperSW (1 + a) xs
    | otherwise = computeDeeperSW a xs