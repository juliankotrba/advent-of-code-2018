import InputHelper
import qualified Data.Set as S (Set,singleton,member,insert)

main = do
    part1
    part2

part1 = do
    content <- readFile "input.txt"
    let input = [ filterPlus content]
    mapM_ (\s -> print $ solve1 $ vListOfInt s) input

part2 = do
    content <- readFile "input.txt"
    let input = [ filterPlus content]
    mapM_ (\s -> print $ solve2 $ vListOfInt s) input

solve1 :: [Int] -> Int
solve1 is = foldr (+) 0 is  

solve2 :: [Int] -> Int
solve2 is = solve2Helper is is (S.singleton 0) 0

type Encountered = S.Set Int
type CurrentSum = Int
type Input = [Int]
type OriginalInput = [Int]

solve2Helper :: OriginalInput -> Input -> Encountered -> CurrentSum -> Int
solve2Helper oi [] es sum = solve2Helper oi oi es sum  
solve2Helper oi inp es sum = 
    let ns = sum + (head inp) -- new sum
        ni = tail inp -- new input
        ne = S.insert ns es -- new encountered
    in
        if (S.member ns es) then ns else solve2Helper oi ni ne ns
   

filterPlus :: String -> String
filterPlus s = filter (/='+') s 

simpleInput1 = "+1, -1"
simpleInput2 = "+3, +3, +4, -2, -4"
simpleInput3 = "-6, +3, +8, +5, -6"
simpleInput4 = "+7, +7, -2, -7, -4" 

allSimpleInputs = [simpleInput1, simpleInput2, simpleInput3, simpleInput4] 