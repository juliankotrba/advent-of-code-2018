import InputHelper
import Data.List.Split
import Data.List
import Data.Char
import qualified Data.Set as S

main = do
    bothParts

bothParts = do
    content <- readFile "input.txt"
    let input = lines content
    let seconds = findSeconds (maxBound::Int) ((maxBound::Int)-1) input 9000
    let matrix = solve1 input seconds
    putStr $ (unlines matrix ++ "Seconds: " ++ show seconds ++ "\n" )

type Seconds = Int    
solve1 :: [String] -> Seconds -> [[Char]]
solve1 xs s = 
    let
        parsed = map parse xs
        afterSeconds = map (calcAfterSeconds s) parsed
        matrix = buildM $ map (\(a,b)->a) afterSeconds
    in
        filter (\xs -> elem '#' xs) matrix

type Width = Int
type WidthBefore = Width 
type Count = Int  
-- TODO: Improve search      
findSeconds :: WidthBefore -> Width -> [String] -> Count -> Count
findSeconds widthBefore currentWidth inp count 
        | currentWidth > widthBefore = count
        | otherwise =
            let
                parsed = map parse inp
                afterSeconds = map (calcAfterSeconds count) parsed
                matrix = buildM $ map (\(a,b)->a) afterSeconds
                newWidth = length $ matrix !! 0
            in
                findSeconds currentWidth newWidth inp (count+1)

calcAfterSeconds :: Seconds -> ((Int, Int),(Int,Int)) -> ((Int, Int),(Int,Int))      
calcAfterSeconds s ((x,y),(xv,yv)) = 
    let
        newX = x + (xv*s)
        newY = y + (yv*s)
    in           
        ((newX,newY),(xv,yv))


parse :: String -> ((Int, Int),(Int,Int))
parse s = 
    let
        -- position=< 7,  0> velocity=<-1,  0>
        coords = splitOn "," (take 14 (drop 10  s))
        x = read $ coords !! 0
        y = read $ coords !! 1

        offs = splitOn "," $ take 6 $ drop 36 s
        xo = read $ offs !! 0
        yo = read $ offs !! 1
    in
        ((x,y),(xo,yo))    

normalize :: [(Int,Int)] -> [(Int,Int)]
normalize is =
    let
        xmin = minimumBy (\(x1,_) (x2,_) -> compare x1 x2) is
        ymin = minimumBy (\(_,y1) (_,y2) -> compare y1 y2) is
    in 
        map (\(x,y) -> ((x+ abs (fst xmin)), (y+ abs (snd ymin)))) is        

bounds :: [(Int,Int)] -> (Int,Int)
bounds cs =
    let 
        x = maximum $ map fst cs
        y = maximum $ map snd cs     
    in
        (x,y) 
                
buildM :: [(Int,Int)] -> [[Char]]
buildM inp = 
    let
        normalizedInput = normalize inp
        bnds = bounds normalizedInput
        c = fst bnds
        r = snd bnds
        set = S.fromList normalizedInput
    in
        [ [(if S.member (x,y) set then '#' else '.') | x<-[0..c]] | y<-[0..r] ]  
