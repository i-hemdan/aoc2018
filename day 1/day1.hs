module Main where

import System.IO
import qualified Data.Map.Strict as Map

main = do   f <- readFile "input.txt"
            let frequencies = parse $ words f
                resulting_frequency = resultingFrequency frequencies in
                    do  putStrLn $ show resulting_frequency
                        putStrLn $ show $ firstDupeF 0 Map.empty frequencies frequencies

parse::[String] -> [Int]
parse s = [ case n of   ('+':rest) -> read rest
                        ('-':rest) -> (-(read rest )) | n <- s ]

resultingFrequency::[Int] -> Int
resultingFrequency l = foldl (+) 0 l

firstDupeF::Int -> Map.Map Int Bool -> [Int] -> [Int] -> Int
firstDupeF  frequency frequency_map working_list original_list =
                case working_list of    [] -> firstDupeF frequency frequency_map original_list original_list
                                        (_:rest)    -> let  f = frequency + head working_list
                                                            f_in_map = Map.lookup f frequency_map in 
                                                                case f_in_map of
                                                                    Just True   -> f
                                                                    Nothing     ->  let new_map = Map.insert f True frequency_map in 
                                                                                        firstDupeF f new_map rest original_list