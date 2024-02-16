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
                            (fromJust linesValue)
                            (fromJust windowValue)
                            (fromJust moveValue)

displayWolfram' :: [Bool] -> [Bool] -> Int -> Int -> Int -> Int -> IO ()
displayWolfram' _ _ _ 0 _ _ = return ()
displayWolfram' rule' (x:xs) start' lines' window' move' = do
    printChar (init xs) start'
    putStrLn ""
    displayWolfram' rule' (generateLine (x:xs) rule')
        start' (lines' - 1) window' move'
displayWolfram' _ _ _ _ _ _ = return ()

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
