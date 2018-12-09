import InputHelper
import Data.Array
import qualified Data.Map as M

part1 = do
    print $ solve1 10 1618
    print $ solve1 13 7999
    print $ solve1 17 1104
    print $ solve1 21 6111
    print $ solve1 30 5807
    print $ solve1 425 70848

-- TODO: Use proper data structure     
part2 = do
    print $ solve1 425 (7084800)
    
type PlayerCount = Int   
type MaxMarbleCount = Int

solve1 :: PlayerCount -> MaxMarbleCount -> Int
solve1 pc mc = solve1Helper 1 0 mc 1 pc [0] M.empty

type Round = Int
type PlayerId = Int
type MaxPlayerId = Int
type CurrentIndex = Int
type CurrentLength = Int
type MaxIndex = Int
type SelectedIndex = Int


solve1Helper :: Round -> CurrentIndex -> MaxIndex -> PlayerId -> MaxPlayerId -> [Int] -> M.Map Int Int -> Int
solve1Helper round i maxIndex p maxPlayer circle scoresMap
    | round > maxIndex = maximum $ M.elems scoresMap
    | (round `mod` 23 /= 0 || round == 1) = 
        let 
            newIndex =  let ni = (i+2) `mod` (length circle) in if ni == 0 then length circle else ni
            newCircle = let ni = (i+2) `mod` (length circle) in if ni == 0 then circle++[round]  else insertAt ni round circle
        in
            solve1Helper (round+1) newIndex maxIndex (if p+1 > maxPlayer then 1 else p+1) maxPlayer newCircle scoresMap    
    | otherwise = 
        let
            newIndex = i - 7
            adjustedIndex =  if newIndex < 0 then (length circle) + newIndex else newIndex
            removedValue = circle !! adjustedIndex
            newCircle = deleteAt adjustedIndex circle
            newScores = if M.member p scoresMap then M.adjust ((round + removedValue)+) p scoresMap else M.insert p (round + removedValue) scoresMap
        in
            solve1Helper (round+1) adjustedIndex maxIndex (if p+1 > maxPlayer then 1 else (p+1)) maxPlayer newCircle newScores    


insertAt :: Int -> Int-> [Int] -> [Int] 
insertAt z y xs = as ++ (y:bs)
    where (as,bs) = splitAt z xs

deleteAt n xs = let (a, b) = splitAt n xs in a ++ tail b    

 
 