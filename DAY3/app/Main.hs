module Main where

import Control.Monad
import Data.Char
import Data.Bool

main = do
    contents <- getContents
    --print (lines contents)
    --print (map strToBoolList (lines contents))
    let inputStream = map strToBoolList (lines contents)
    let result = (diagnostic [] inputStream)
    let gamma = (boolBinToDec (reverse (tupleToBool (result))))
    let epsilon = (boolBinToDec (reverse (tupleToNegBool (result))))
    print (gamma * epsilon)
    let oxyRate = (filterForOxy inputStream)
    let cO2Rate = (filterForCO2 inputStream)
    print (oxyRate * cO2Rate)

filterForOxy :: [[Bool]] -> Int
filterForOxy [x] = boolBinToDec (reverse x)
filterForOxy x = filterForOxy (oxyRatingFiltBin (calcFiltMax (diagnostic [] x)) x )

filterForCO2 :: [[Bool]] -> Int
filterForCO2 [x] = boolBinToDec (reverse x)
filterForCO2 x = filterForCO2 (oxyRatingFiltBin (calcFiltMin (diagnostic [] x)) x )

calcFiltMax :: [(Int, Int)] -> [Bool]
calcFiltMax [x] = [bool False True (a >= b) | (a, b) <- [x]]
calcFiltMax (x:xs)
    | (a + b == 1 ) && (a >= b) = [True]
    | (a + b == 1 ) && (a < b) = [False]
    | (a * b /= 0) && (a >= b) = [True] 
    | (a * b /= 0) && (a < b) = [False] 
    | (a * b == 0) && (a > b) = True : calcFiltMax xs
    | (a * b == 0) && (a < b) = False : calcFiltMax xs
    where (a, b) = x

calcFiltMin :: [(Int, Int)] -> [Bool]
calcFiltMin [x] = [bool True False (a >= b) | (a, b) <- [x]]
calcFiltMin (x:xs)
    | (a + b == 1 ) && (a < b) = [True]
    | (a + b == 1 ) && (a >= b) = [False]
    | (a * b /= 0) && (a < b) = [True] 
    | (a * b /= 0) && (a >= b) = [False] 
    | (a * b == 0) && (a > b) = True : calcFiltMin xs
    | (a * b == 0) && (a < b) = False : calcFiltMin xs
    where (a, b) = x

oxyRatingFiltBin:: [Bool] -> [[Bool]] -> [[Bool]]
oxyRatingFiltBin x [y] = [y]
oxyRatingFiltBin [x] y = [a | a <- y, (take 1 a) == [x]]
oxyRatingFiltBin x y = [a | a <- y, (take (length x) a) == x]

diagnostic :: [(Int, Int)] -> [[Bool]] -> [(Int, Int)]
diagnostic [] [x] = [(bool 0 1 a, bool 1 0 a) | a <- x] 
diagnostic [] (x:xs) = diagnostic [(bool 0 1 a, bool 1 0 a) | a <- x] xs
diagnostic x [y] = countBools x (boolToTuple y)
diagnostic x (y:ys) = diagnostic (countBools x (boolToTuple y)) ys

countBools :: [(Int, Int)] -> [(Int, Int)]-> [(Int, Int)]
countBools [(a, b)] [(c, d)] = [(a + c, b + d)]
countBools ((a, b):xs) ((c, d):ys) = (a + c, b + d) : countBools xs ys

boolToTuple :: [Bool] -> [(Int, Int)]
boolToTuple [True] = [(1, 0)]
boolToTuple [False] = [(0, 1)]
boolToTuple (x:xs) = bool (0, 1) (1, 0) x : boolToTuple xs

strToBoolList :: String -> [Bool]
strToBoolList "1" = [True]
strToBoolList "0" = [False]
strToBoolList (x:xs) = bool False True (x == '1') : strToBoolList xs

tupleToBool :: [(Int, Int)] -> [Bool]
tupleToBool [(a, b)] = [bool False True (a > b)]
tupleToBool ((a, b):xs) = bool False True (a > b) : tupleToBool xs


tupleToNegBool :: [(Int, Int)] -> [Bool]
tupleToNegBool [(a, b)] = [bool True False (a > b)]
tupleToNegBool ((a, b):xs) = bool True False (a > b) : tupleToNegBool xs

boolBinToDec :: [Bool] -> Int
boolBinToDec = foldr (\x y -> fromEnum x + 2*y) 0
