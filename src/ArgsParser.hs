{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- argsParser
-}

module ArgsParser
    (
        parseArgs,
        checkConfigurationValues,
        printError,
    ) where

import Text.Read (readMaybe)
import Data.Maybe (isJust, isNothing, fromJust)
import System.Exit (exitWith, ExitCode(ExitFailure))
import GHC.IO.Handle (hPutStr)
import GHC.IO.Handle.FD (stderr)
import Conf (Conf(..))

atoi :: String -> Maybe Int
atoi = readMaybe

getRule :: [String] -> Maybe Int
getRule ("--rule":x:_) | isJust (atoi x) = atoi x
                       | otherwise = Nothing
getRule (_:args) = getRule args
getRule [] = Nothing

getStart :: [String] -> Maybe Int
getStart ("--start":x:_) | isJust (atoi x) && fromJust (atoi x) < 0 = Nothing
                        | isJust (atoi x) = atoi x
                        | otherwise = Nothing
getStart ("--start":_) = Nothing
getStart (_:args) = getStart args
getStart [] = Just 0

getLines :: [String] -> Maybe Int
getLines ("--lines":x:_) | isJust (atoi x) && fromJust (atoi x) < 0 = Nothing
                        | isJust (atoi x) = atoi x
                        | otherwise = Nothing
getLines ("--lines":_) = Nothing
getLines (_:args) = getLines args
getLines [] = Just (-1)

getWindow :: [String] -> Maybe Int
getWindow ("--window":x:_)   | isJust (atoi x) && fromJust (atoi x) < 0 = Nothing
                            | isJust (atoi x) = atoi x
                            | otherwise = Nothing
getWindow ("--window":_) = Nothing
getWindow (_:args) = getWindow args
getWindow [] = Just 80

getMove :: [String] -> Maybe Int
getMove ("--move":x:_) | isJust (atoi x) = atoi x
                      | otherwise = Nothing
getMove ("--move":_) = Nothing
getMove (_:args) = getMove args
getMove [] = Just 0

parseArgs :: [String] -> Conf
parseArgs args = Conf {
    rule = getRule args,
    start = getStart args,
    confLines = getLines args,
    window = getWindow args,
    move = getMove args
}

printError :: String -> IO ()
printError str = hPutStr stderr ("Error: " ++ str) >> exitWith (ExitFailure 84)

checkConfigurationValues :: Conf -> IO ()
checkConfigurationValues (Conf
    ruleValue startValue linesValue
    windowValue moveValue)  | isNothing ruleValue = printError "Invalid rule"
                    | fromJust ruleValue /= 30 && fromJust ruleValue /= 90
                        && fromJust ruleValue /= 110 = printError "Invalid rule"
                    | isNothing startValue = printError "Invalid start"
                    | isNothing linesValue = printError "Invalid lines"
                    | isNothing windowValue = printError "Invalid window"
                    | isNothing moveValue = printError "Invalid move"
                    | otherwise = return ()

