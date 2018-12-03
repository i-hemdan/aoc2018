module Main where

import System.IO
import qualified Data.Map.Strict as Map

main = do
    f <- readFile "input.txt"
    let
        frequencies = parse $ words f
        resulting_frequency = resultingFrequency frequencies
        in
            putStrLn $ show resulting_frequency
        

parse::[String] -> [Int]
parse s = 
    [ case n of
            ('+':rest) -> read rest
            ('-':rest) -> (-(read rest ))
        | n <- s ]

        
resultingFrequency::[Int] -> Int
resultingFrequency l = foldl (+) 0 l

firstDupeF =
