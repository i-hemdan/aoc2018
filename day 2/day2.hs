module Main where


import System.IO
import qualified Data.Map.Strict as Map


main =  do  
        f <- readFile "input.txt"
        let ids = words f in
            putStrLn $ show $ hasExactlyN "aabbbccc" 'a' 1 0


hasExactlyN::String -> Char -> Int -> Int -> Bool
hasExactlyN string char count accum = case string of
    []          ->  case accum of       accum | accum == count  -> True 
                                        _                       -> False

    (x:rest)    ->  case x of           x | x == char   -> hasExactlyN rest char count (accum+1)
                                        _               -> hasExactlyN rest char count accum



part1 list twos threes = undefined