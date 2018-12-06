import InputHelper
import Data.List.Split
import Data.List
import qualified Data.Set as S

main = do
    part1
    part2

part1 = do
    content <- readFile "input.txt"
    let input = parse content
    print $ solve1 input
    
solve1 :: [(Char, (Int,Int))] -> Int
solve1 input = 
    let
        bnds = bounds $ map snd input
        matrix = buildM1 input (fst bnds) (snd bnds)
        bndValues = S.toList $ boundValues matrix
     in
        (sortBy (flip compare) $ map (length) $ group $ sort $ removeAll bndValues (concat matrix))!!0   

part2 = do
    content <- readFile "input.txt"
    let input = parse content
    --let input = parse simpleInput
    print $ solve2 input
    
          
solve2 :: [(Char, (Int,Int))] -> Int
solve2 input = 
    let
        bnds = bounds $ map snd input
        matrix = buildM2 input (fst bnds) (snd bnds)
    in
        length $ filter (\c-> c=='#') $ concat matrix
        
        
boundValues :: [[Char]] -> S.Set Char
boundValues matrix =
    let
        top = matrix !! 0
        bottom = matrix !! ((length matrix)-1)
        left = map head matrix
        right = map (\r -> r !! ((length r)-1)) matrix
    in
        S.fromList $ top ++ bottom ++ left ++ right

bounds :: [(Int,Int)] -> (Int,Int)
bounds cs =
    let 
        x = maximum $ map fst cs
        y = maximum $ map snd cs
     
    in
        (x,y) 
        
buildM1 :: [(Char, (Int,Int))] -> Int -> Int -> [[Char]]
buildM1 inp c r = [ [(calcChar1 inp (x,y)) | x<-[0..c+1]] | y<-[0..r] ]

buildM2 :: [(Char, (Int,Int))] -> Int -> Int -> [[Char]]
buildM2 inp c r = [ [(calcChar2 inp (x,y)) | x<-[0..c+1]] | y<-[0..r] ]

calcChar2 :: [(Char, (Int,Int))] -> (Int, Int) -> Char
calcChar2 inp coord = 
    let
        mans = map (\(c, xy) -> (c, manDist xy coord)) inp
        su = sum $ (map snd mans)
        min = minimumBy (\(_,dist1) (_,dist2) -> compare dist1 dist2) mans
        mins = filter (\(_,dist) -> dist == (snd min)) mans
    in
        if su < 10000 then '#' else  if length mins == 1 then fst min else '.'

calcChar1 :: [(Char, (Int,Int))] -> (Int, Int) -> Char
calcChar1 inp coord = 
    let
        mans = map (\(c, xy) -> (c, manDist xy coord)) inp
        min = minimumBy (\(_,dist1) (_,dist2) -> compare dist1 dist2) mans
        mins = filter (\(_,dist) -> dist == (snd min)) mans
    in
        if length mins == 1 then fst min else '.'

manDist :: (Int,Int) -> (Int,Int) -> Int
manDist (x1,y1) (x2,y2) =  abs (x2-x1) + abs (y2-y1)

parse :: String -> [(Char, (Int,Int))]
parse s = zip allAlphabet  $ map (\s-> (read $ (splitOn ", " s)!!0 :: Int,  read $ (splitOn ", " s)!!1 :: Int)) $ lines $ s

allAlphabet :: [Char]
allAlphabet = [ c | c<- ['A'..'Z']] ++ [ c | c<- ['a'..'z']]

removeAll :: [Char] -> [Char] -> [Char]
removeAll cs ls = [ x | x <- ls, not (x `elem` cs) ]       

simpleInput = "1, 1\n\
\1, 6\n\
\8, 3\n\
\3, 4\n\
\5, 5\n\
\8, 9"


 
 