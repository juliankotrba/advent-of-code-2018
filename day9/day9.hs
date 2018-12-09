import InputHelper
import Data.Array
import qualified Data.Map as M

part1 = do
    print $ solve1 425 70848

type PlayerCount = Int   
type MaxMarbleCount = Int

solve1 :: PlayerCount -> MaxMarbleCount -> Int
solve1 pc mc = maximum $ (M.elems (solve1Helper 1 0 mc 1 pc [0] M.empty))

type Round = Int
type PlayerId = Int
type MaxPlayerId = Int
type CurrentIndex = Int
type CurrentLength = Int
type MaxIndex = Int
type SelectedIndex = Int


solve1Helper :: Round -> CurrentIndex -> MaxIndex -> PlayerId -> MaxPlayerId -> [Int] -> M.Map Int Int -> M.Map Int Int
solve1Helper round i maxIndex p maxPlayer circle scoresMap
    | round > maxIndex = scoresMap
    | p > maxPlayer = solve1Helper round i maxIndex 1 maxPlayer circle scoresMap
    | otherwise = 
        if (round `mod` 23 /= 0 || round == 1) then
            let 
                newIndex =  let ni = (i+2) `mod` (length circle) in if ni == 0 then length circle else ni
                newCircle = let ni = (i+2) `mod` (length circle) in if ni == 0 then circle++[round]  else insertAt ni round circle
            in
                solve1Helper (round+1) newIndex maxIndex (p+1) maxPlayer newCircle scoresMap    
        else 
            let
                newIndex = i - 7
                adjustedIndex =  if newIndex < 0 then (length circle) + newIndex else newIndex
                removedValue = circle !! adjustedIndex
                newCircle = deleteAt adjustedIndex circle
                newScores = if M.member p scoresMap then M.adjust ((round + removedValue)+) p scoresMap else M.insert p (round + removedValue) scoresMap
            in
                if adjustedIndex < 0 then error "<0" else solve1Helper (round+1) adjustedIndex maxIndex (p+1) maxPlayer newCircle newScores    


insertAt :: Int -> Int-> [Int] -> [Int] 
insertAt z y xs = as ++ (y:bs)
    where (as,bs) = splitAt z xs

deleteAt :: Int -> [a] -> [a]
deleteAt _ []     = []
deleteAt i (a:as)
    | i == 0    = as
    | otherwise = a : deleteAt (i-1) as    

 
 