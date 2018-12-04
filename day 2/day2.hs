module Main where


import System.IO
import qualified Data.Map.Strict as Map


main =  do  
        f <- readFile "input.txt"
        let ids = words f in
            do
                putStrLn $ show $ calcCheckSum $ useHasOfOnInput ids (0,0)
                putStrLn $ show $ checkDifs ids ids ""


hasOf :: String -> Map.Map Char Int -> [(Char, Int)]
hasOf string m = case string of
    [] -> Map.toList m
    (x:xs) -> let new_map = Map.insertWith (+) x 1 m in hasOf xs new_map

useHasOfOnInput:: [String] -> (Int, Int)  -> (Int, Int)
useHasOfOnInput input out = case input of
    [] -> out
    (x:xs) -> let   tarr = hasOf x Map.empty 
                    twos_threes = splitTwosThrees tarr
                    in
                        case twos_threes of
                            ((x1:xs1), (x2:xs2))-> let new_out = addTup out (1, 1) in useHasOfOnInput xs new_out
                            ([], (x1:xs1))      -> let new_out = addTup out (0, 1) in useHasOfOnInput xs new_out
                            ((x1:xs1), [])      -> let new_out = addTup out (1, 0) in useHasOfOnInput xs new_out
                            ([], [])            -> useHasOfOnInput xs out

splitTwosThrees:: [(Char, Int)] -> ([(Char, Int)], [(Char, Int)])
splitTwosThrees ls = let    ls1 = [el | el <- ls, let (_, x) = el in x == 2]
                            ls2 = [el2 | el2 <- ls, let (_, x2) = el2 in x2 == 3]
                            in (ls1, ls2)

calcCheckSum tuple = let (a, b) = tuple in a * b

lsJoin:: [a] -> [a] -> [a]
lsJoin a b = case a of
    [] -> b
    (x:xs) -> lsJoin xs (x:b)      

addTup a b = 
    let (w, x) = a
        (y, z) = b
        in ((w + y), (x + z))


--dangerously assumes both strings are the same length
difString:: String -> String -> Int -> String -> (Int, String)
difString stra strb countDiff comp =
    case stra of
        [] -> (countDiff, (reverse comp))
        (x:xs) ->
            let (x1:xs1) = strb in
                case x1 of
                    x1 | x1 /= x -> difString xs xs1 (countDiff+1) comp
                    x1 | x1 == x -> difString xs xs1 countDiff (x:comp)

--bad
checkDifs::[String] -> [String] -> String -> String
checkDifs orig work curString =
    case work of
        [] -> let (_:o) = orig in checkDifs o o ""
        otherwise ->
            case curString of
                [] -> let (x:xs) = work in checkDifs orig xs x
                otherwise -> let    (x:xs) = work
                                    s = difString x curString 0 ""
                                    (numDif, str) = s
                                    in
                                        case numDif of
                                            n | n == 1 -> 
                                                case str of
                                                    str | (length str) == ((length x)-1) -> str
                                            otherwise ->checkDifs orig xs curString