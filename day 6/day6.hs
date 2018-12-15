module Main where

import Data.List
import Data.List.Split

main = readFile "input.txt" >>= (\f -> putStrLn $ show $ genPlane $ sort $ parsePoints f)

calcAllDists ls ls2 = [(P x y [(b, manD a b) | b<-ls2]) | a@(P x y _)<-ls]

genPlane ls = let   ((P fx fy _):_) = ls
                    ((P lx ly _):_) = reverse ls
                    (xmod, ymod) = (abs (fx-lx), abs (fy-ly))
                    (begin, end) = ((P (fx-xmod) (fy-ymod) []), (P (lx+xmod) (ly+ymod) [])) in genFromTo begin end

parsePoints str = [P (read a::Int) (read b::Int) []|s<-(splitOn "\n" str), let (a:b:[])=(splitOn ", " s)]

manD (P x1 y1 _) (P x2 y2 _) = (abs $ x1 - x2) + (abs $ y1 - y2)

data Point = P Int Int [(Point, Int)] deriving(Show)
instance Eq Point where (P x y _) == (P x1 y1 _) = (x == x1) && (y == y1)
instance Ord Point where compare (P x y _) (P x1 y1 _)|y < y1 = LT    |y == y1 && x < x1 = LT    |y == y1 && x == x1 = EQ    |otherwise = GT

genFromTo (P x1 y1 _) (P x2 y2 _) = [(P x y [])| y<-[y1..y2], x<-[x1..x2]]

flipPs ls = [flipP p|p<-ls] where flipP (P x y l) = (P y x l)

addP (P x1 y1 l) (P x2 y2 _) = (P (x1+x2) (y1+y2) l)