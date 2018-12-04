module Main where


import Data.List.Split


main = putStrLn . show $ new_fabric 1000 1000


--Joint fabric and claim stuff
--TODO: doit
--Joint fabric and claim stuff end

--Claim stuff
data Claim = Claim {
    cl_id::String,
    cl_pos:: (Int, Int),
    cl_dim:: (Int, Int)
    }deriving (Show)

readClaim str = 
    let getId ((Claim _ pos dim), str) = let a = splitOn "@" str in ((Claim (a!!0) pos dim), (a!!1))

        getPos ((Claim id _ dim), str) = 
            let a = splitOn ":" str in let b = splitOn "," (a!!0)in ((Claim id ( (read (b!!0))+1 , (read(b!!1))+1 ) dim), (a!!1))

        getDim ((Claim id pos _), str) = let a = splitOn "x" str in ((Claim id pos ((read $ a!!0), (read $ a!!1))), str)

        readClaim' = getDim . getPos . getId

        (claim, _) = readClaim' ( (Claim "" (0,0) (0,0)) , str )
        in claim


inputToClaimList:: String -> [Claim]
inputToClaimList input = 
    let ls = (splitLines input) in
        let inputToClaimList' ls1 nls =
                case ls1 of
                    [] -> nls
                    (x:xs) -> inputToClaimList' xs ((readClaim x):nls)
                in inputToClaimList' ls []

--Claim stuff end
--Fabric stuff
data Fabric = Fabric {
    fab_width::Int,
    fab_height::Int,
    fab_squares::[SquareInch]
}deriving (Show)

new_fabric w h = (Fabric w h [ (SquareInch []) | e <- [1 .. w*h]])

getInch (Fabric w h a) x y = a!!((w*y) + x)

setInch (Fabric w h a) x y inch =  let new_a = updatels a ((w*y) + x) inch [] in (Fabric w h new_a)

data SquareInch = SquareInch {
    si_claim_ids :: [String]
}deriving (Show)
--Fabric stuff end
--Util stuff
updatels (h:tl) i elem accul =
    case i of
        i | i == 0 -> (reverse accul) ++ (elem:tl)
        otherwise -> updatels tl (i-1) elem (h:accul)

splitLines str = splitOn "\n" str
--Util stuff end