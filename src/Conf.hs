{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Conf
-}

module Conf (Conf(..)) where

data Conf = Conf
    { rule :: Maybe Int
    , start :: Maybe Int
    , confLines :: Maybe Int
    , window :: Maybe Int
    , move :: Maybe Int
    } deriving (Show)
