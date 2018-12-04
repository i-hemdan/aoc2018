module Main where


import Data.List.Split


main = putStrLn $ show $ readClaim "#1 @ 1,3: 4x4"



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
    claim_id::String,
    claim_pos:: (Int, Int),
    claim_dim:: (Int, Int)
    }deriving (Show)