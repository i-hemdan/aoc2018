module Main where

import Data.List.Split

main = readFile "input.txt" >>= (\f -> putStrLn $ show $ parse f)


parse str = [let (a:b:[])=(splitOn ", " s) in (read a,read b)::(Int, Int)| s<-(splitOn "\n" str)]