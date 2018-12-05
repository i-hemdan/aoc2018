module Main where

import Data.List.Split
import qualified Data.Map.Strict as Map





main = putStrLn "tst"



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
--      TimeStamp
instance Eq TimeStamp where
    (TimeStamp y m d h mi ) == (TimeStamp y2 m2 d2 h2 mi2) = 
        fin1 == fin2
        where
            fin1 = (y*12*30*24*60)+(m*30*24*60)+(d*24*60)+(mi)
            fin2 = (y2*12*30*24*60)+(m2*30*24*60)+(d2*24*60)+(mi2)
--      Eq TimeStamp