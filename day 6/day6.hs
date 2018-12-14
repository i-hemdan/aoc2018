module Main where

import Data.List.Split



data Point a = P a a deriving(Show)

main = readFile "input.txt" >>= (\f -> putStrLn $ show $ parse f)

parse str = [let (a:b:[])=(splitOn ", " s) in P (read a::Int) (read b::Int)| s<-(splitOn "\n" str)]
manD (P x1 y1) (P x2 y2) = (abs $ x1 - x2) + (abs $ y1 - y2)
