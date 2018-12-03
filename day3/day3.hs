import InputHelper
import Data.List.Split
import Data.List
import qualified Data.Set as S (Set,empty,member,insert, fromList, size)

part1 = do
    content <- readFile "input.txt"
    let input = lines $ content
    --let input = allSimpleInputs
    print $ solve1 input

solve1 :: [String] -> Int
solve1 xs = countOverlaps $ foldr (++) [] $ map allCoordinates $ map toClaim xs

countOverlaps :: Coordinates -> Int
countOverlaps cs = countOverlapsHelper cs S.empty S.empty 0

type CurrentCount = Int

countOverlapsHelper :: Coordinates -> S.Set (Int,Int) -> S.Set (Int,Int) -> CurrentCount -> Int
countOverlapsHelper [] _ _ cc = cc
countOverlapsHelper cs s s2 cc = 
    let
        h = head cs
    in
        if ((S.member h s) && not (S.member h s2)) 
            then 
                countOverlapsHelper (tail cs) s (S.insert h s2) (cc+1) 
            else 
                countOverlapsHelper (tail cs) (S.insert h s) s2 cc     

type Coordinates = [(Int, Int)]

allCoordinates :: Claim -> Coordinates
allCoordinates c = allCoordiantesHelper c (height c) []
 
type CurrentHeight = Int
type CurrentCoordinates = Coordinates

allCoordiantesHelper :: Claim -> CurrentHeight -> CurrentCoordinates -> Coordinates
allCoordiantesHelper _ 0 cc = cc
allCoordiantesHelper c ch cc = 
    let
        coordiantes = [(x,(startY c) + ch -1) | x <- [(startX c) .. (startX c) + (width c) -1] ]
    in
        allCoordiantesHelper c (ch-1) cc++coordiantes

data Claim = Claim { id :: String  
                     , startX :: Int  
                     , startY :: Int  
                     , width :: Int  
                     , height :: Int  
                    } deriving (Show, Eq)
                    
-- #123 @ 3,2: 5x4
toClaim :: String -> Claim
toClaim s = 
    let
        atSplit = splitOn "@" (filter (/=' ') s)
        id = atSplit !! 0
        
        starts = (splitOn ":" (atSplit !! 1)) !! 0
        lengths = (splitOn ":" (atSplit !! 1)) !! 1

        startX = read $ (splitOn "," starts) !! 0
        startY = read $ (splitOn "," starts) !! 1
        width = read $ (splitOn "x" lengths) !! 0
        height = read $ (splitOn "x" lengths) !! 1
    in
        Claim { Main.id = id, startX = startX, startY = startY, width = width, height = height}


part2 = do
    content <- readFile "input.txt"
    let input = lines $ content
    let all = map allCoordinates $ map toClaim input
    let mapped = map (\xs -> map (\ys -> if (anyDifferent xs ys && xs /= ys) then [] else xs) all) all
    print (filter (not .containsEmpty) mapped !! 0 !!0 )

containsEmpty :: [[a]] -> Bool
containsEmpty xs = any (\ys -> length ys == 0) xs 

anyDifferent :: (Eq a) => [a] -> [a] -> Bool
anyDifferent a = not . null . intersect a

simpleInput1 = "#1 @ 1,3: 4x4"
simpleInput2 = "#2 @ 3,1: 4x4"
simpleInput3 = "#3 @ 5,5: 2x2" 

allSimpleInputs = [simpleInput1, simpleInput2, simpleInput3] 

 
 