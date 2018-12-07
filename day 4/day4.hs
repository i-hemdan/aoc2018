{-# LANGUAGE ParallelListComp #-}
module Main where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map





main =  readFile "input4.txt" >>= (\f -> putStrLn $ show $ doIt f)


doIt =
    conv
    . Map.toList
    . (sepGActions Map.empty) 
    . parseActions 
    . sortParsedTS 
    . parseTimeStamps 
    . splitLines

data Action = Action Integer String ActionType deriving (Show)
data MiniAction = S Integer|W Integer deriving (Show)
data ActionType = BeginShift|Sleep|Wake deriving (Show)

conv ls = [(id,(convMaToRanges l))| (id,l) <- ls]

convMaToRanges ls = 
    let 
        sleeps = [e| e@(S _)<-ls]
        wakes  = [e| e@(W _)<-ls]
        lens = length sleeps
        lenw = length wakes
        in
            case (lens,lenw) of
                (lens,lenw)| lens /= lenw -> (error $ "Not equal lens" ++ (show lens) ++ "lenw" ++ (show lenw))
                otherwise -> [ (s,w) | (S s) <- sleeps | (W w) <- wakes ]
            


sepGActions m ls =
    case ls of
        [] -> m
        (x:xs) ->
            case x of
                (Action _ _ BeginShift) -> sepGActions m xs
                ((Action mi id at)) -> 
                    let newm = (Map.insertWith (++) id [(conv (Action mi id at))] m) 
                        in  sepGActions newm xs
                            where
                                conv (Action mi _ Sleep) = (S mi)
                                conv (Action mi _ Wake)  = (W mi)


parseActions ls = 
    let arr =[(parseAction n)| n<-ls]
        in go arr [] ""
        where
            go [] new_arr _ = new_arr
            go (x:xs) new_arr id =
                case x of
                    (Action ts new_id BeginShift)   -> go xs (x:new_arr) new_id
                    (Action ts _ act)               -> go xs ((Action ts id act):new_arr) id

            parseAction::(TimeStamp, [String]) -> Action
            parseAction (ts, strArr) =
                case strArr of
                    ["Guard", id, _, _] -> (Action (ts_minute ts) id BeginShift)
                    ["falls", _]        -> (Action (ts_minute ts) "" Sleep)
                    ["wakes", _]        -> (Action (ts_minute ts) "" Wake)

parseTimeStamps::[String] -> [(TimeStamp, [String])]
parseTimeStamps ls =
    let parseTimeStamps' ls new_ls =
            case ls of  [] ->   new_ls
                        (x:xs) -> parseTimeStamps' xs ((parseTimeStamp x):new_ls)
        in parseTimeStamps' ls []

parseTimeStamp:: String -> (TimeStamp, [String])
parseTimeStamp str =
    let (bdate:timeb:rest) = splitOn " " str
        (_:date) = bdate
        time = take 5 timeb 
        (y:m:d:_) = splitOn "-" date
        (h:mi:_) = splitOn ":" time
        (yn, mn, dn, hn, min) = (read y, read m, read d, read h, read mi)
        ts = (TimeStamp yn mn dn hn min)
        in (ts, rest)

sortParsedTS:: [(TimeStamp, [String])] -> [(TimeStamp, [String])]
sortParsedTS tstupls = sortBy (\(a,_)(b,_) -> compare a b) tstupls

data    TimeStamp = TimeStamp  
                    { ts_year   ::Integer
                    , ts_month  ::Integer
                    , ts_day    ::Integer
                    , ts_hour   ::Integer
                    , ts_minute ::Integer 
                    } deriving (Show)

toMinutes (TimeStamp y m d h mi) = (y*12*30*24*60)+(m*30*24*60)+(d*24*60)+(mi)

fromMinutes::Integer -> TimeStamp
fromMinutes 0 = (TimeStamp 0 0 0 0 0)
fromMinutes mi = let
                    m1 = mi
                    (y, m2) = divWithRem mi (12*30*24*60)
                    (m, m3) = divWithRem m2 (30*24*60)
                    (d, m4) = divWithRem m3 (24*60)
                    (h, m5) = divWithRem m4 (60)
                    in (TimeStamp y m d h m5)
divWithRem::Integer -> Integer -> (Integer, Integer)
divWithRem _ 0 = (0,0)                    
divWithRem a d = ((div a d),(rem a d)) 

instance Eq TimeStamp where
    t1@(TimeStamp y m d h mi ) == t2@(TimeStamp y2 m2 d2 h2 mi2) = 
        fin1 == fin2
        where
            fin1 = toMinutes t1
            fin2 = toMinutes t2

instance Num TimeStamp where
    t1 + t2 = fromMinutes( (toMinutes t1) + (toMinutes t2) )
    t1 - t2 = fromMinutes( (toMinutes t1) - (toMinutes t2) )


instance Ord TimeStamp where
    compare t1 t2 = compare (toMinutes t1) (toMinutes t2)

--Utils
splitLines:: String -> [String]
splitLines str = splitOn "\n" str

joinSets:: [[Integer]] -> [Integer]
joinSets ls =
    go ls []
    where
        go [] l = sort l
        go (x:xs) l = go xs (l++x)
    
getMost:: [Integer] -> Integer
getMost ls =
    let g = group ls
        in  go g 0 0
            where   go:: [[Integer]] ->Integer ->Integer ->Integer
                    go [] _ n1 = n1
                    go (x:xs) l1 n1 =
                        case (toInteger (length x)) of
                            len | len > l1  -> go xs len (x!!0)
                            otherwise       -> go xs l1 n1
getMostOfSets = getMost.joinSets

-- Utils end