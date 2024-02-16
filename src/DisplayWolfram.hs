{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- DisplayWolfram
-}

module DisplayWolfram
    (
        displayWolfram
    ) where

import Data.Maybe (fromJust)
import Conf (Conf(..))

displayWolfram :: Conf -> IO ()
displayWolfram (Conf
    ruleValue startValue linesValue
    windowValue moveValue) = displayWolfram'
                            (getRuleAsBinary (fromJust ruleValue))
                            (generateWholeStart windowValue)
                            (fromJust startValue)
                            (fromJust linesValue + fromJust startValue)
                            (fromJust windowValue)
                            (fromJust moveValue)
                            1

displayWolfram' :: [Bool] -> [Bool] -> Int -> Int -> Int -> Int -> Int -> IO ()
displayWolfram' _ _ _ 0 _ _ _ = return ()
displayWolfram' rule' pattern' start' lines' window' move' lineCount
    | lineCount < start' =
        displayWolfram' rule' (generateLine pattern' rule')
            start' (lines' - 1) window' move' (lineCount + 1)
    | otherwise =
        printLine (removeOffset pattern' lineCount) start' move' >>
        putStrLn "" >>
        displayWolfram' rule' (generateLine pattern' rule')
            start' (lines' - 1) window' move' (lineCount + 1)

printLine :: [Bool] -> Int -> Int -> IO ()
printLine [] _ _ = return ()
printLine (x:xs) start' move' | move' < 0 =
                        printLine (xs ++ [x]) start' (move' + 1)
                              | move' > 0 =
                        printLine (last xs : init (x:xs)) start' (move' - 1)
                              | x = putStr "*" >> printLine xs start' 0
                              | otherwise = putStr " " >> printLine xs start' 0

getRuleAsBinary :: Int -> [Bool]
getRuleAsBinary x = reverse (take 8 (getRuleAsBinary' x ++ repeat False))

getRuleAsBinary' :: Int -> [Bool]
getRuleAsBinary' 0 = []
getRuleAsBinary' x = (x `mod` 2 == 1) : getRuleAsBinary' (x `div` 2)

generateStart :: Int -> Int -> [Bool]
generateStart 0 _ = []
generateStart x y | x == y = True : generateStart (x - 1) y
                  | otherwise = False : generateStart (x - 1) y

generateWholeStart :: Maybe Int -> [Bool]
generateWholeStart Nothing = []
generateWholeStart (Just x) | x `mod` 2 == 1 =
                        False : (generateStart x (x `div` 2 + 1) ++ [False])
                            | otherwise =
                        False : (generateStart x (x `div` 2) ++ [False])

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

generateLine :: [Bool] -> [Bool] -> [Bool]
generateLine [] _ = []
generateLine x y = False : generateLine' (False : x ++ [False]) y ++ [False]

generateLine' :: [Bool] -> [Bool] -> [Bool]
generateLine' (one:two:three:xs) y | null xs = [getPattern y [one, two, three]]
                                   | otherwise = getPattern y [one, two, three]
                                            : generateLine' (two:three:xs) y
generateLine' _ _ = []


removeOffset :: [Bool] -> Int -> [Bool]
removeOffset x 0 = x
removeOffset (_:xs) n = removeOffset (init xs) (n - 1)
removeOffset _ _ = []
