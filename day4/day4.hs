import InputHelper
import Data.List.Split
import Data.List
import Data.Ord
import qualified Data.Map.Strict as M (Map, empty, insert,fromList,member,adjust,filter,elems,keys,(!),toList)

part1 = do
    content <- readFile "input.txt"
    let input = lines $ content
   --let input = lines $ simpleInput
    print (solve1 input)

solve1 :: [String] -> (String, Int)
solve1 xs = 
    let
        allMap = toMap $ sortRecords $ map toRecord xs
        maxId = getMaxId $allMap
        listOfSleeps = allMap M.! maxId
        allMinutesAsSortedList =  sort $ foldr (++) [] (map (\(x,y) -> [x..y-1]) listOfSleeps)
        most = (maximumBy (comparing length) (groupBy (==) allMinutesAsSortedList))  
    in
        (maxId, most!!0) 

part2 = do
    content <- readFile "input.txt"
    let input = lines $ content
    --let input = lines $ simpleInput
    let result = solve2 input
    print result
        
solve2 :: [String] -> [(String, [CurrentStart])]
solve2 xs = 
    let
        allMap = toMap $ sortRecords $ map toRecord xs
        allList = M.toList allMap
        mapped = map (\(k,v) ->  (k, (maximumBy (comparing length) (groupBy (==) (sort $ foldr (++) [] (map (\(x,y) -> [x..y-1]) v) ))))) allList
    in
        mapped


type CurrentId = String
type CurrentStart = Int
type CurrentEnd = Int

getMaxId :: M.Map String [(CurrentStart,CurrentEnd)] -> String
getMaxId m = getMaxIdHelper (M.keys m) "" 0 m
 
getMaxIdHelper :: [String] -> String -> Int -> M.Map String [(CurrentStart,CurrentEnd)] -> String
getMaxIdHelper [] currId _ _ = currId
getMaxIdHelper ids currId currMax m = 
    let
        id = head ids
        minutes = m M.! id
        sum = foldr (+) 0 (map (\(x,y) -> y-x) minutes)
    in
        getMaxIdHelper (tail ids) (if sum > currMax then id else currId) (if sum > currMax then sum else currMax) m

toMap :: [Record] -> M.Map String [(CurrentStart,CurrentEnd)]
toMap rs = toMapHelper rs "" 0 0 M.empty

toMapHelper :: [Record] -> CurrentId -> CurrentStart -> CurrentEnd -> M.Map String [(CurrentStart,CurrentEnd)] -> M.Map String [(CurrentStart,CurrentEnd)]
toMapHelper [] id s e m = m
toMapHelper rs id s e m = 
    let
        r = head rs  
    in
        if (isPrefixOf "Guard" (action r)) then toMapHelper (tail rs) (action r) s e  (if (M.member (action r) m) then m else (M.insert (action r) [] m )) else
            if (isPrefixOf "falls" (action r)) then toMapHelper (tail rs) id (minutes r) e m else
                toMapHelper (tail rs) id s e (M.adjust ((s,(minutes r)) :) id m)     


toRecord :: String -> Record
toRecord s = 
    let
        timestamp = (splitOn "]" s) !! 0
        action = (splitOn "] " s) !! 1
        minutes = read $ take 2 ((splitOn ":" s) !! 1)
    in
        Record { timestamp = timestamp, minutes= minutes, action = action}    

data Record = Record { timestamp :: String  
                    ,minutes :: Int  
                    ,action :: String   
                    } deriving (Show, Eq)  
sortRecords :: [Record] -> [Record]
sortRecords rs = sortBy (\x y -> compareRecords x y) rs

compareRecords :: Record -> Record -> Ordering
compareRecords r1 r2 = compare (timestamp r1) (timestamp r2) 

simpleInput = "[1518-11-01 00:00] Guard #10 begins shift\n\
\[1518-11-03 00:05] Guard #10 begins shift\n\
\[1518-11-01 00:25] wakes up\n\
\[1518-11-01 00:30] falls asleep\n\
\[1518-11-01 00:55] wakes up\n\
\[1518-11-01 23:58] Guard #99 begins shift\n\
\[1518-11-02 00:40] falls asleep\n\
\[1518-11-02 00:50] wakes up\n\
\[1518-11-03 00:24] falls asleep\n\
\[1518-11-03 00:29] wakes up\n\
\[1518-11-04 00:02] Guard #99 begins shift\n\
\[1518-11-04 00:36] falls asleep\n\
\[1518-11-04 00:46] wakes up\n\
\[1518-11-05 00:03] Guard #99 begins shift\n\
\[1518-11-05 00:45] falls asleep\n\
\[1518-11-01 00:05] falls asleep\n\
\[1518-11-05 00:55] wakes up"

allSimpleInputs = [simpleInput] 

 
 