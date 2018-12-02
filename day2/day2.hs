import InputHelper
import Data.Map (fromListWith, toList)

main = do
    part1
    part2

part1 = do
    content <- readFile "input.txt"
    let input = lines $ content
    --let input = allSimpleInput
    print (solve1 input)

solve1 :: [String] -> Int
solve1 is = 
    let
        twos = length $ filter (not . null) (fmap (\xs -> filterWithCount 2 xs) is)
        threes = length $ filter (not . null) (fmap (\xs -> filterWithCount 3 xs) is)
    in
        twos * threes

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

filterWithCount c xs = filter (\(_,count) -> count == c) (frequency xs)

part2 = do
    content <- readFile "input.txt"
    print $ solve2 $ lines $ content 

solve2 :: [String] -> String
solve2 xs = 
    let 
        -- TODO: simplify!
        unfilteredAlmostEquals = fmap (\s1 -> fmap (\s2-> if (almostEqual s1 s2) then (s1,s2) else ("","") ) xs ) xs
        filtered = filter (\p -> p /= ("","")) (filter (any (\e -> e /= ("",""))) unfilteredAlmostEquals !! 0) !! 0
    in
        commonLetters filtered 

almostEqual :: String -> String -> Bool
almostEqual s1 s2 = almostEqualHelper s1 s2 0 

commonLetters :: (String, String) -> String
commonLetters p = commonLettersHelper p ""

commonLettersHelper :: (String, String) -> String -> String
commonLettersHelper (s1,s2) cur
        | length s1 == 0 = cur
        | otherwise = commonLettersHelper (tail s1, tail s2) (cur ++ (if head s1 == head s2 then ([head s1]) else ""))

type WrongCount = Int

almostEqualHelper :: String -> String -> WrongCount -> Bool
almostEqualHelper s1 s2 c
    | c > 1 = False
    | length s1 == 0 && c == 0 = False
    | length s1 == 0 && c == 1 = True
    | otherwise = 
        let 
            count = if ((head s1) /= (head s2)) then c+1 else c
        in
            almostEqualHelper (tail s1) (tail s2) count


simpleInput1 = "abcde"
simpleInput2 = "fghij"
simpleInput3 = "klmno" 
simpleInput4 = "pqrst" 
simpleInput5 = "fguij" 
simpleInput6 = "axcye" 
simpleInput7 = "wvxyz"

allSimpleInput = [simpleInput1, simpleInput2, simpleInput3, simpleInput4, simpleInput5, simpleInput6, simpleInput7] 

 
 