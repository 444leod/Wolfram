{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Main
-}

module Main (main) where

import ArgsParser
import DisplayWolfram

import System.Environment (getArgs)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    args <- getArgs
    let conf = parseArgs args
    checkConfigurationValues conf
    displayWolfram conf
    exitSuccess
