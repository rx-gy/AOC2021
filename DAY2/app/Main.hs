module Main where

import Control.Monad
import Data.Char
import Data.Text.Read

main = do
    contents <- getContents
    --print (lines contents)
    let (horizontal, depth) = (computeLoc (0, 0) (lines contents))
    do
        print (horizontal, depth)
        print (horizontal * depth)
    let (horizontal, depth, aim) = (computeLoc2 (0, 0, 0) (lines contents))
    do
        print (horizontal, depth, aim)
        print (horizontal * depth)


-- (current hor, current depth) -> [instructions] -> (new hor, new depth)
computeLoc :: (Int, Int) -> [String] -> (Int, Int)
computeLoc (h, d) [instruction]
    | take 8 instruction == "forward " =  (h + read (drop 8 instruction), d) 
    | take 5 instruction == "down " = (h, d + read (drop 5 instruction))
    | take 3 instruction == "up " = (h, d - read (drop 3 instruction))
    | otherwise = (h, d)
computeLoc (h, d) (instruction:xs)
    | take 8 instruction == "forward " =  computeLoc (h + read (drop 8 instruction), d) xs
    | take 5 instruction == "down " = computeLoc (h, d + read (drop 5 instruction)) xs
    | take 3 instruction == "up " = computeLoc (h, d - read (drop 3 instruction)) xs
    | otherwise = computeLoc (h, d) xs


-- (current hor, current depth, current aim) -> [instructions] -> (new hor, new depth, new aim)
computeLoc2 :: (Int, Int, Int) -> [String] -> (Int, Int, Int)
computeLoc2 (h, d, a) [instruction]
    | take 8 instruction == "forward " =  (h + read (drop 8 instruction), (d + (a * read (drop 8 instruction))), a) 
    | take 5 instruction == "down " = (h, d, a + read (drop 5 instruction))
    | take 3 instruction == "up " = (h, d, a - read (drop 3 instruction))
    | otherwise = (h, d, a)
computeLoc2 (h, d, a) (instruction:xs)
    | take 8 instruction == "forward " =  computeLoc2 (h + read (drop 8 instruction), (d + (a * read (drop 8 instruction))), a) xs
    | take 5 instruction == "down " = computeLoc2 (h, d, a + read (drop 5 instruction)) xs
    | take 3 instruction == "up " = computeLoc2 (h, d, a - read (drop 3 instruction)) xs
    | otherwise = computeLoc2 (h, d, a) xs
