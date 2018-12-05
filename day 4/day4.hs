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
                    { ts_year   ::Int
                    , ts_month  ::Int
                    , ts_day    ::Int
                    , ts_hour   ::Int
                    , ts_minute ::Int }
--      TimeStamp
