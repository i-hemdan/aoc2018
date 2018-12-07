module Main where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map





main =  readFile "input.txt" >>= (\f -> putStrLn $ show $ doIt f)


doIt = getGuardsFromShifts [] . splitShifts . getAllActions . sortParsedTS . parseTimeStamps . splitLines


--                 |id    |asleep |most asleep minute
data Guard = Guard String Integer [Integer] deriving (Show)
data Shift = Shift [Action] deriving (Show)

showShifts ls =
    go ls "" where      go [] news = news
                        go (x:xs) news = go xs (news++"\n\n"++(show x))
    
        
--TODO structure shifts into individual guards

idFromShift (Shift ((BeginShift id _):_)) = id

--assuming even length
findMostSleptMinute::Guard -> Guard
findMostSleptMinute guard@(Guard id tasleep arr) =
    let
        asleepMin = [(-n) | n<-arr, n < 0]
        awakeMin = [n | n<-arr, n >= 0]
        minPairs = zip asleepMin awakeMin
        minRanges = [[a..b] | (a,b)<-minPairs]
        most = getMostOfSets minRanges
        in
            (Guard id tasleep [most])

getGuardsFromShifts:: [Guard] ->[Shift] -> [Guard]
getGuardsFromShifts guards shifts =
    case shifts of
        [] -> (reverse guards)
        (x:xs) -> getGuardsFromShifts ((go x (Guard "" 0 [0])):guards) xs
        where
            go::Shift -> Guard -> Guard
            go (Shift []) g = (findMostSleptMinute g)
            go (Shift (hd:tl)) guard = 
                case hd of
                    (BeginShift id _) -> go (Shift tl) (Guard id 0 [0])
                    (Asleep (TimeStamp _ _ _ _ mi)) -> 
                        let 
                            g@(Guard id maslp mostaslp) = guard 
                            in
                                go (Shift tl) (Guard id (maslp - mi) ((-mi):mostaslp))
                    (WakesUp (TimeStamp _ _ _ _ mi)) ->
                        let
                            g@(Guard id maslp mostaslp) = guard
                            in
                                go (Shift tl) (Guard id (maslp + mi) (mi:mostaslp))

getGuardIdsFromShifts::[Shift] -> [String]
getGuardIdsFromShifts ls = 
    go ls []
    where
        go [] l2 = l2
        go ((shif):xs) l2 = 
            case (idFromShift shif) of
                id1 | id1 `elem` l2 -> go xs l2
                otherwise -> go xs ((idFromShift shif):l2)

splitShifts:: [Action] -> [Shift]
splitShifts ls =
    go ls [] where      go:: [Action] -> [Shift] -> [Shift] --assumes data starts with a begin shift
                        go [] l =  (reverse l) --done
                        go (x:xs) [] = go xs ((Shift (x:[])):[])--if the list of shifts is empty, start with a new shift assuming the data starts with a begin shift
                        go (x:[]) (hd@(Shift arr):tl) = go [] ((Shift (reverse (x:arr))):tl)--if last action ensure the final shift action list is reversed into the proper order
                        go (x:xs) (hd@(Shift arr):tl) = case x of   beg@(BeginShift _ _) -> go xs ((Shift (beg:[])):(((Shift (reverse arr))):tl))--reverse the previous array to keep proper order and start new shift
                                                                    x -> go xs ((Shift (x:arr)):tl)--add events to a shift  

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

--      shift

data    Action =    BeginShift String TimeStamp |
                    Asleep TimeStamp            |
                    WakesUp TimeStamp
                    deriving (Show)

getAllActions ls = 
    go ls []
    where
        go [] new_ls = (reverse new_ls)
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

--      TimeStamp

--Utils
splitLines:: String -> [String]
splitLines str = splitOn "\n" str

joinSets:: [[Integer]] -> [Integer]
joinSets ls =
    go ls []
    where
        go [] l = l
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