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
displayWolfram (Conf
    ruleValue startValue linesValue
    windowValue moveValue) = wolfram
                            (getRuleAsBinary (fromJust ruleValue))
                            (False : generateStartLine (fromJust windowValue) ++ [False])
                            (fromJust startValue)
                            (fromJust linesValue + fromJust startValue)
                            (fromJust windowValue)
                            (fromJust moveValue)
                            1

wolfram :: [Bool] -> [Bool] -> Int -> Int -> Int -> Int -> Int -> IO ()
wolfram _ _ 0 0 _ _ _ = return ()
wolfram rule pattern 0 lines window move currentLine =
    printLine pattern move window currentLine >>
        wolfram
            rule (False : generateLine currentPattern rule ++ [False])
            0 (lines - 1)
            window move
            (currentLine + 1)
    where currentPattern = removeBorder pattern
wolfram rule pattern start lines window move currentLine =
    wolfram
        rule (generateLine (removeBorder pattern) rule)
        (start - 1) lines
        window move (currentLine + 1)

removeBorder :: [Bool] -> [Bool]
removeBorder [] = []
removeBorder (_:xs) = xs

generateLine :: [Bool] -> [Bool] -> [Bool]
generateLine [] _ = []
generateLine x y = False : getPatternedLine (False : x ++ [False]) y ++ [False]

getPatternedLine :: [Bool] -> [Bool] -> [Bool]
getPatternedLine [a, b, c] y = [getPattern y [a, b, c]]
getPatternedLine (a:b:c:xs) y
    = getPattern y [a, b, c] : getPatternedLine (b:c:xs) y
getPatternedLine _ _ = []

generateStartLine :: Int  -> [Bool]
generateStartLine size = generateStartLine' size (size `div` 2)

generateStartLine' :: Int -> Int -> [Bool]
generateStartLine' 0 _ = []
generateStartLine' x y  | x == y = True : generateStartLine' (x - 1) y
                        | otherwise = False : generateStartLine' (x - 1) y

printLine :: [Bool] -> Int -> Int -> Int -> IO ()
printLine (x:xs) move window line   | move > 0 = printLine (False : x : xs) (move - 1) window line
                                    | move < 0 = printLine (xs ++ [False]) (move + 1) window line
                                    | otherwise = printLine' (x:xs) window line
printLine _ _ _ _ = return ()

printLine' :: [Bool] -> Int -> Int -> IO ()
printLine' list window 0 = printFullLine list window
printLine' (_:xs) window line = printLine' xs window (line - 1)
printLine' _ _ _ = return ()

printFullLine :: [Bool] -> Int -> IO ()
printFullLine _ 0 = putStrLn ""
printFullLine (True:xs) window = putStr "*" >> printFullLine xs (window - 1)
printFullLine (False:xs) window = putStr " " >> printFullLine xs (window - 1)
printFullLine _ _ = return ()

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
