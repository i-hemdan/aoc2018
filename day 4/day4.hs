module Main where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map





main = 
    do 
        f <- readFile "input.txt"
        putStrLn $ show $ doIt f
        where
            doIt = getAllActions . sortParsedTS . parseTimeStamps . splitLines



data Guard = Guard String [Shift]
data Shift = Shift [Action]

splitShifts ls =
    go ls []
    where
        go [] ls = ls
        go (beg@(BeginShift _ _):xs) ls = go xs ((Shift beg:[]):ls)
        go (x:xs) (hd:tl) =
            case x of
                slp@(Asleep _) -> undefined
                wak@(WakesUp _) -> undefined


parseTimeStamps::[String] -> [(TimeStamp, [String])]
parseTimeStamps ls =
    let parseTimeStamps' ls new_ls =
            case ls of
                [] -> new_ls
                (x:xs) -> parseTimeStamps' xs ((parseTimeStamp x):new_ls)
        in
            parseTimeStamps' ls []


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

--      shift

data    Action =    BeginShift String TimeStamp |
                    Asleep TimeStamp            |
                    WakesUp TimeStamp
                    deriving (Show)

getAllActions ls = 
    go ls []
    where
        go [] new_ls = new_ls
        go (x:xs) new_ls = go xs ((actionFromTsLs x):new_ls)

actionFromTsLs (_, []) = error "empty list in actionFromTsLs"
actionFromTsLs (ts, (hd:tl)) =
    case hd of
        ("Guard") -> BeginShift (tl!!0) ts
        ("falls") -> Asleep ts
        ("wakes") -> WakesUp ts
        otherwise -> error "invalid action"

--      Action   

data    TimeStamp = TimeStamp  
                    { ts_year   ::Integer
                    , ts_month  ::Integer
                    , ts_day    ::Integer
                    , ts_hour   ::Integer
                    , ts_minute ::Integer 
                    } deriving (Show)

toMinutes (TimeStamp y m d h mi) = (y*12*30*24*60)+(m*30*24*60)+(d*24*60)+(mi)

instance Eq TimeStamp where
    t1@(TimeStamp y m d h mi ) == t2@(TimeStamp y2 m2 d2 h2 mi2) = 
        fin1 == fin2
        where
            fin1 = toMinutes t1
            fin2 = toMinutes t2

instance Ord TimeStamp where
    compare t1 t2 = compare (toMinutes t1) (toMinutes t2)

--      TimeStamp

--Utils
splitLines:: String -> [String]
splitLines str = splitOn "\n" str

-- Utils end