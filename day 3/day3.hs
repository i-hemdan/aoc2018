module Main where


import Data.List.Split


main = putStrLn $ show $ updatels [1,2,3,4] 1 3 []

splitLines str = splitOn "\n" str

readClaim str = 
    let
        getId ((Claim _ pos dim), str) = let a = splitOn "@" str in ((Claim (a!!0) pos dim), (a!!1))

        getPos ((Claim id _ dim), str) = 
            let a = splitOn ":" str in let b = splitOn "," (a!!0)in ((Claim id ((read (b!!0)), (read(b!!1))) dim), (a!!1))

        getDim ((Claim id pos _), str) = let a = splitOn "x" str in ((Claim id pos ((read $ a!!0), (read $ a!!1))), str)

        readClaim' = getDim . getPos . getId

        (claim, _) = readClaim' ( (Claim "" (0,0) (0,0)) , str )
        in
            claim

data Claim = Claim {
    cl_id::String,
    cl_pos:: (Int, Int),
    cl_dim:: (Int, Int)
    }deriving (Show)

data Fabric = Fabric {
    fab_width::Int,
    fab_height::Int,
    fab_squares::[SquareInch]
}deriving (Show)

getInch (Fabric w h a) x y = a!!((w*y) + x)

setInch (Fabric w h a) x y inch =  let new_a = updatels a ((w*y) + x) inch [] in (Fabric w h new_a)

data SquareInch = SquareInch {
    si_claim_ids :: [String]
}deriving (Show)


updatels (h:tl) i elem accul =
    case i of
        i | i == 0 -> (reverse accul) ++ (elem:tl)
        otherwise -> updatels tl (i-1) elem (h:accul)