module Main where

import Data.List.Split
import qualified Data.Map.Strict as Map





main = putStrLn $ show $ (TimeStamp 2018 12 4 12 05) > (TimeStamp 2018 12 5 12 05)



data    Event = Event
                { e_time    ::TimeStamp
                , e_guard   ::String
                , e_action  ::Action }
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
                    , ts_minute ::Integer }

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
