import InputHelper

part1 = do
    content <- readFile "input.txt"
    --let input = [content]
    --let input = allSimpleInputs
    mapM_ (\s -> print $ solve1 $ hListOfInt s ",") input

solve1 :: [Int] -> Int
solve1 is = foldr (+) 0 is     

part2 = do
    print "TODO"

simpleInput1 = "1,2,3"
simpleInput2 = "1,2,3"
simpleInput3 = "1,2,3" 

allSimpleInputs = [simpleInput1, simpleInput2, simpleInput3] 

 
 