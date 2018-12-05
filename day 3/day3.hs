module Main where


import Data.List.Split
import qualified Data.Map.Strict as Map


main = do 
    f <- readFile "input.txt"
    let 
        claims = f
        claimList = inputToClaimList claims
        idsquares = claimListToIdSquares claimList
        fabric = new_fabric 1000 1000
        fabric1 = populateFabricInchesWithClaims idsquares fabric
        checker = squareToPosList 0 0 1000 1000
        count = countNorMoreClaims 2 checker fabric1 0
        in
            do
                putStrLn $ show count
                putStrLn $ findUniqueClaim idsquares fabric1


--Joint fabric and claim stuff
countNorMoreClaims n ls (Fabric w h m) a =
    case ls of
        []-> a
        (hd:tl) -> 
            let 
                Just (SquareInch ids) = Map.lookup hd m
                in
                    case ids of
                        ids|(length ids) >= n -> countNorMoreClaims n tl (Fabric w h m) (a+1)
                        otherwise -> countNorMoreClaims n tl (Fabric w h m) a

findUniqueClaim idSquares fabric =
    case idSquares of
        [] -> "fail"
        ((id, ls):rest) -> 
            let 
                c = countNorMoreClaims 2 ls fabric 0
                in
                    case c of
                        c| c == 0 -> id
                        otherwise -> findUniqueClaim rest fabric




populateFabricInchesWithClaims idRects fabric =
    let 
        popu rects f =
            case rects of
                [] -> f
                ((id, ls):rest) -> popu rest (pop id ls f)
        pop id ins f2 =
            case ins of
                [] -> f2
                (pos:rl) -> pop id rl (addIdToSquareInchOfFabric id pos f2)
        in popu idRects fabric
            

addIdToSquareInchOfFabric id pos f@(Fabric fw fh fm) =
    let Just (SquareInch arr) = Map.lookup pos fm
        new_arr = id:arr
        new_fm = Map.insert pos (SquareInch new_arr) fm
        in (Fabric fw fh new_fm)

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

claimListToIdSquares claims =
    let f c l =
            case c of
                [] -> l
                ((Claim id (x,y) (w,h)):tl) -> f tl ((id, (squareToPosList x y w h)):l)
            in f claims []

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
    fab_squares::Map.Map (Int, Int) SquareInch
}deriving (Show)

new_fabric w h = 
    let gen wid l =
            case wid of
                wid | wid-1 >= 0 -> gen (wid-1) (gen' (wid-1) (h-1) l)
                otherwise -> l
        gen' x y l =
            case y of
                y | y >= 0 -> gen' x (y - 1) (((x, y), (SquareInch [])):l)
                otherwise -> l
        in (Fabric w h (Map.fromList $ gen w []))


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

squareToPosList x y w h =
    let genLine w1 h1 ls =
            case w1 of
                w1|w1 > x -> genLine (w1-1) h1 (((w1-1), (h1-1)):ls)
                w1|w1 == x -> ls
        genRows w2 h2 ls =
            case h2 of
                h2|h2 > y -> genRows w2 (h2-1) ((genLine w2 h2 [])++ls)
                h2|h2 == y -> ls
        in genRows (x+w) (y+h) []
--Util stuff end