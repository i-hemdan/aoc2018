module Main where
import qualified Data.Char as C
import Data.List
import Data.Function (on)

main = readFile "input.txt" >>= (\f -> putStrLn $ show $ (length $ sol f, part2 f))

sol str =   let go s1 s2    | (length s1) == (length s2) = s1
                            | otherwise = go (condense s1) s1 in go str ""

condense str =  go str ""   where   go [] s2                            = s2
                                    go (hd:[]) s2                       = (hd:s2)
                                    go (hd:pk:[]) s2    |comp hd pk     = s2
                                                        |otherwise      = pk:hd:s2
                                    go (hd:pk:tl) s2    |comp hd pk     = go tl (s2)
                                                        |otherwise      = go (pk:tl) (hd:s2)

comp a b    |a /= b && (C.toUpper a) == (C.toUpper b)   = True
            |otherwise                                  = False

part2 str = let answer = sortBy (compare `on` length) [sol $ filter (/=(C.toUpper letter)) $ filter (/=letter) str|letter<-['a'..'z']] in length (answer!!0)