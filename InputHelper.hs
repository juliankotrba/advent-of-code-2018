module InputHelper 
( hListOfInt
, hListOfDouble
, vListOfInt
, vListOfDouble
, intMatrix
, doubleMatrix
) 
where

import System.IO
import Data.List.Split

type Seperator = String
	
intMatrix :: String -> Seperator -> [[Int]]
intMatrix s sep = map (\line -> hListOfInt line sep) (lines s)

doubleMatrix :: String -> Seperator -> [[Double]]
doubleMatrix s sep = map (\line -> hListOfDouble line sep) (lines s)

-- horizontal list like 1,2,3,4,..

hListOfInt :: String -> Seperator -> [Int]
hListOfInt s sep = hListOfHelper s sep :: [Int]

hListOfDouble :: String -> Seperator -> [Double]
hListOfDouble s sep = hListOfHelper s sep :: [Double]

hListOfHelper :: (Read a) => String -> Seperator -> [a] 
hListOfHelper s sep = fmap (\s -> read s) (splitOn sep s)

{- vertical list like   1
					    2
						3
-}

vListOfInt :: String -> [Int]
vListOfInt s = vListOfHelper s :: [Int]

vListOfDouble :: String -> [Double]
vListOfDouble s = vListOfHelper s :: [Double]

vListOfHelper :: (Read a) => String -> [a] 
vListOfHelper s = fmap read (lines s)