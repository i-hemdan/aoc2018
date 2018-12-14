module Main where

import Data.List
import Data.List.Split



data Point = P Int Int deriving(Show)
instance Eq Point where
    (P x y) == (P x1 y1) = (x == x1) && (y == y1)
instance Ord Point where
    compare (P x y) (P x1 y1)|y < y1 = LT|y == y1 && x < x1 = LT|y == y1 && x == x1 = EQ|otherwise = GT

main = readFile "input.txt" >>= (\f -> putStrLn $ show $ sort $ parse f)

parse str = [P (read a::Int) (read b::Int)|s<-(splitOn "\n" str), let (a:b:[])=(splitOn ", " s)]
manD (P x1 y1) (P x2 y2) = (abs $ x1 - x2) + (abs $ y1 - y2)
