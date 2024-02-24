{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- DisplayWolfram
-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module DisplayWolfram
    (
        displayWolfram
    ) where

import Data.Maybe (fromJust)
import Conf (Conf(..))

displayWolfram :: Conf -> IO ()
displayWolfram (Conf ruleValue startValue linesValue
    windowValue moveValue) = wolfram
                            (getRuleAsBinary (fromJust ruleValue))
                            (generateStartLine (fromJust windowValue + 2))
                            (fromJust startValue)
                            (fromJust linesValue + fromJust startValue)
                            (fromJust windowValue)
                            (fromJust moveValue)
                            1

wolfram :: [Bool] -> [Bool] -> Int -> Int -> Int -> Int -> Int -> IO ()
wolfram _ _ 0 0 _ _ _ = return ()
wolfram rule pattern 0 lines window move currentLine =
    wolframWithoutStart rule pattern lines window move currentLine
wolfram rule pattern start lines window move currentLine =
    wolfram
        rule (False : generateLine currentPattern rule)
            (start - 1) (lines - 1)
            window move
            (currentLine + 1)
    where currentPattern = removeBorder pattern

wolframWithoutStart :: [Bool] -> [Bool] -> Int -> Int -> Int -> Int -> IO ()
wolframWithoutStart _ _ 0 _ _ _ = return ()
wolframWithoutStart rule pattern lines window move currentLine =
        printFullLine
            (getCutLine (getMovedLine pattern move) currentLine) window >>
        wolframWithoutStart rule
            (False : generateLine currentPattern rule) (lines - 1)
            window move
            (currentLine + 1)
    where currentPattern = removeBorder pattern

removeBorder :: [Bool] -> [Bool]
removeBorder [] = []
removeBorder (_:xs) = xs

generateLine :: [Bool] -> [Bool] -> [Bool]
generateLine [] _ = []
generateLine x y = False : getPatternedLine (False : x) y

getPatternedLine :: [Bool] -> [Bool] -> [Bool]
getPatternedLine [a, b] y = [getPattern y [a, b, False], False, False, False]
getPatternedLine [a, b, c] y = getPattern y [a, b, c] : getPatternedLine [b, c] y
getPatternedLine (a:b:c:xs) y
    = getPattern y [a, b, c] : getPatternedLine (b:c:xs) y
getPatternedLine _ _ = []

generateStartLine :: Int  -> [Bool]
generateStartLine size = generateStartLine' size (size `div` 2)

generateStartLine' :: Int -> Int -> [Bool]
generateStartLine' 0 _ = []
generateStartLine' x y  | x == y = True : generateStartLine' (x - 1) y
                        | otherwise = False : generateStartLine' (x - 1) y

getMovedLine :: [Bool] -> Int -> [Bool]
getMovedLine (x:xs) move | move > 0 = getMovedLine (False : (x:xs)) (move - 1)
                         | move < 0 = getMovedLine xs (move + 1)
                         | otherwise = x:xs
getMovedLine [] _ = []

getCutLine :: [Bool] -> Int -> [Bool]
getCutLine list 0 = list
getCutLine (_:xs) currentLine = getCutLine xs (currentLine - 1)
getCutLine _ _ = []

printFullLine :: [Bool] -> Int -> IO ()
printFullLine _ 0 = putStrLn ""
printFullLine [] window = putStr " " >> printFullLine [] (window - 1)
printFullLine (True:xs) window = putStr "*" >> printFullLine xs (window - 1)
printFullLine (False:xs) window = putStr " " >> printFullLine xs (window - 1)

getRuleAsBinary :: Int -> [Bool]
getRuleAsBinary x = reverse (take 8 (getRuleAsBinary' x ++ repeat False))

getRuleAsBinary' :: Int -> [Bool]
getRuleAsBinary' 0 = []
getRuleAsBinary' x = (x `mod` 2 == 1) : getRuleAsBinary' (x `div` 2)

getPattern :: [Bool] -> [Bool] -> Bool
getPattern binary (True:True:True:_) = head binary
getPattern binary (True:True:False:_) = binary !! 1
getPattern binary (True:False:True:_) = binary !! 2
getPattern binary (True:False:False:_) = binary !! 3
getPattern binary (False:True:True:_) = binary !! 4
getPattern binary (False:True:False:_) = binary !! 5
getPattern binary (False:False:True:_) = binary !! 6
getPattern binary (False:False:False:_) = binary !! 7
getPattern _ _ = False
