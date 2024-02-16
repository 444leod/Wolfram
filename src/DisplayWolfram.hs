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
