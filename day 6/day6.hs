module Main where

import Data.List
import Data.List.Split

import qualified Data.Map as M

main = readFile "input.txt" >>= (\f -> putStrLn $ show $ conv [] $ calcAllDists $ genPlane $ sort $ parsePoints f)

sepAreas ls = go ls M.empty where go l m = undefined

conv::[((Int,Int,Bound,Int),(Int,Int))] -> [Point] ->[((Int,Int,Bound,Int),(Int,Int))]
conv nls [] = nls
conv nls ((P x y (((P x1 y1 _ _), distance):[]) b):ls) = conv (((x,y,b,distance),(x1,y1)):nls) ls

calcAllDists (ls, ls2) = 
    map (\(P x y (hd:_) b) -> (P x y [hd] b)) $ filter filterMul [(P x y (sortBy s [(b, manD a b) | b<-ls2]) abound) | a@(P x y _ abound)<-ls] 
    where   s = (\(_,a) (_,b)->compare a b)
            filterMul (P _ _ ((_,a):(_,b):_) _) = a /= b

genPlane ls = let   ((P fx fy _ _):_) = ls
                    ((P lx ly _ _):_) = reverse ls
                    (xmod, ymod) = (abs (fx-lx), abs (fy-ly))
                    (begin, end) = ((P (fx) (fy) [] Fin), (P (lx) (ly) [] Fin)) in ((genFromTo begin end), ls)

parsePoints str = [P (read a::Int) (read b::Int) [] Fin|s<-(splitOn "\n" str), let (a:b:[])=(splitOn ", " s)]

manD (P x1 y1 _ _) (P x2 y2 _ _) = (abs $ x1 - x2) + (abs $ y1 - y2)
data Bound = Inf | Fin deriving(Show)
data Point = P Int Int [(Point, Int)] Bound deriving(Show)
instance Eq Point where (P x y _ _) == (P x1 y1 _ _) = (x == x1) && (y == y1)
instance Ord Point where compare (P x y _ _) (P x1 y1 _ _)|y < y1 = LT    |y == y1 && x < x1 = LT    |y == y1 && x == x1 = EQ    |otherwise = GT

genFromTo (P x1 y1 _ _) (P x2 y2 _ _) = [(P x y [] (checkInf x y x1 y1 x2 y2))| y<-[y1..y2], x<-[x1..x2]]
checkInf x y bx1 by1 bx2 by2 = 
    case (x, y) of
        (x,y)|(x == bx1||x==bx2)||(y==by1||y==by2) -> Inf
        _->Fin

flipPs ls = [flipP p|p<-ls] where flipP (P x y l b) = (P y x l b)

addP (P x1 y1 l b) (P x2 y2 _ _) = (P (x1+x2) (y1+y2) l b)