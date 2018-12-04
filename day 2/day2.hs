module Main where


import System.IO
import qualified Data.Map.Strict as Map


main =  do  
        f <- readFile "input.txt"
        let ids = words f in
            putStrLn $ show $ calcCheckSum $ splitTwosThrees $ useHasOfOnInput ids []


hasOf :: String -> Map.Map Char Int -> [(Char, Int)]
hasOf string m = case string of
    [] -> Map.toList m
    (x:xs) -> let new_map = Map.insertWith (+) x 1 m in hasOf xs new_map

useHasOfOnInput:: [String] -> [(Char, Int)]  -> [(Char, Int)]
useHasOfOnInput input out = case input of
    [] -> out
    (x:xs) -> let   tarr = hasOf x Map.empty 
                    new_out = lsJoin tarr out
                    in
                        useHasOfOnInput xs new_out

splitTwosThrees:: [(Char, Int)] -> ([(Char, Int)], [(Char, Int)])
splitTwosThrees ls = let    ls1 = [el | el <- ls, let (_, x) = el in x == 2]
                            ls2 = [el2 | el2 <- ls, let (_, x2) = el2 in x2 == 3]
                            in
                                (ls1, ls2)

calcCheckSum tuple_list = let   (a, b) = tuple_list
                                lena = length a
                                lenb = length b
                                in
                                    lena * lenb

lsJoin:: [a] -> [a] -> [a]
lsJoin a b = case a of
    [] -> b
    (x:xs) -> lsJoin xs (x:b)      