module Main where

import Data.List.Split
import qualified Data.Map.Strict as Map





main = putStrLn $ show $ parseEvent "[2018-12-05 10:24]"



data    Event = Event
                { e_time    ::TimeStamp
                , e_guard   ::String
                , e_action  ::Action }

parseEvent str =
    let
        parseTimeStamp s1 =
            let (bdate:timeb:rest) = splitOn " " s1
                (_:date) = bdate
                time = take 5 timeb 
                (y:m:d:_) = splitOn "-" date
                (h:mi:_) = splitOn ":" time
                (yn, mn, dn, hn, min) = (read y, read m, read d, read h, read mi)
                ts = (TimeStamp yn mn dn hn min)
                in ts
        in
            parseTimeStamp str
                

--      Event

data    Action =    BeginShift  |
                    Asleep      |
                    WakesUp
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
