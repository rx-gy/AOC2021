module Main where

import Control.Monad
import Data.Char
import Data.Bool
import Data.List
import GHC.Utils.Misc

main = do
    contents <- getContents
    -- print contents
    let allData = lines contents
    let randDraw = [read x :: Int |  x <- split ',' (head allData)]

    let firstGrid = [[read x :: Int | x <- words (allData!!2)], 
                    [read x :: Int | x <- words (allData!!3)], 
                    [read x :: Int | x <- words (allData!!4)], 
                    [read x :: Int | x <- words (allData!!5)], 
                    [read x :: Int | x <- words (allData!!6)]]

    let secondGrid = [[read x :: Int | x <- words (allData!!8)], 
                    [read x :: Int | x <- words (allData!!9)], 
                    [read x :: Int | x <- words (allData!!10)], 
                    [read x :: Int | x <- words (allData!!11)], 
                    [read x :: Int | x <- words (allData!!12)]]

    let thirdGrid = [[read x :: Int | x <- words (allData!!14)], 
                    [read x :: Int | x <- words (allData!!15)], 
                    [read x :: Int | x <- words (allData!!16)], 
                    [read x :: Int | x <- words (allData!!17)], 
                    [read x :: Int | x <- words (allData!!18)]]

    let firstGridP = transpose firstGrid
    let secondGridP = transpose secondGrid
    let thirdGridP = transpose thirdGrid
    
    let allGrids = [firstGrid, firstGridP, secondGrid, secondGridP, thirdGrid, thirdGridP]
    
    print randDraw
    print allGrids

findGridMatch :: [Int] -> [[Int]] -> [Int] -> [[Int]]
findGridMatch x y = [ a | a <- x, b <- y, a `elem` b] 

