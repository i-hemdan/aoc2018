module Main where

import Data.List.Split

data Point a = P a a deriving(Show)

main = readFile "input.txt" >>= (\f -> putStrLn $ show $ parse f)


parse str = [let (a:b:[])=(splitOn ", " s) in P (read a::Int) (read b::Int)| s<-(splitOn "\n" str)]