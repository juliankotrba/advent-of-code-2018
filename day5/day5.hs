import InputHelper
import Data.List
import Data.Char
import Data.List.Split
import Data.Ord
import qualified Data.Text as T

main = do
    part1
    part2

part1 = do
    content <- readFile "input.txt"
    let input = content
    --let input = simpleInput
    print $ solve1 input

solve1 :: String -> Int
solve1 is = whileHasReacting (-1) oneReaction is  

part2 = do
    content <- readFile "input.txt"
    let input = content
    --let input = simpleInput
    print $ solve2 input

solve2 :: String -> Int
solve2 ps = minimum 
                $ map solve1 
                    $ filter (\polymer-> length polymer /= length ps) 
                        $ map (\c -> (removeUnits $ [c]++[(toUpper c)]) ps) [c | c<-['a'..'z']]

removeUnits us ls = [ x | x <- ls, not (x `elem` us) ]

-- TODO: Improve runtime
oneReaction xs = concat $ map (\ls -> if (length ls >= 2) then drop 2 ls else ls ) $ groupBy invertEquals xs

invertEquals :: Char -> Char -> Bool
invertEquals c1 c2 = c1 == (invertCase c2) || c2 == (invertCase c1)

invertCase :: Char -> Char
invertCase c = if isLower c then toUpper c else toLower c

whileHasReacting :: Int -> (String -> String) -> String -> Int
whileHasReacting i f s
    | i == (length s) = length s 
    | otherwise =  whileHasReacting (length s) f (f s)    

simpleInput = "dabAcCaCBAcCcaDA"


 
 