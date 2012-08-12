-- Shane Fitzpatrick 09487581
-- Module describes a Record data type and handles the loading of a file.
-- Each row in the file is represented with this Record and the entire file is 
-- represented by a list of Records
module CmdFile where

import qualified Data.Text as T
import Data.Maybe
import Data.List
import qualified Utility as U

data Record = Record	{ row :: Int
						, club :: String
						, mapName :: String
						, nearestTown :: String
						, terrain :: String
						, mapGrade :: String
						, gridRefOfSWCorner :: String
						, gridRefOfNECorner :: String
						, expectedCompletionDate :: String
						, sizeSqKm :: String
						} | Null deriving (Show, Eq, Ord)					
				
-- Takes in a string of the entire csv file and coverts it to a list of recrods				
load :: String -> [Record]				
load file = getRecords 1 $ dropIncompRows $ dropEmptyRows $ map (take 9) $ U.dropNull $ map U.mergeQuotes $ unPack $ fields $ mLines file

-- Returns a list of all the lines, split on \r or \n				
mLines :: String -> [T.Text]		
mLines text = concatMap (T.splitOn (T.pack "\r")) (nLines $ T.pack text)

nLines :: T.Text -> [T.Text]
nLines text = T.splitOn (T.pack "\n") text
				
-- list of rows, each with the values split into sections
fields :: [T.Text] -> [[T.Text]]	
fields (x:xs) = map (T.splitOn $ T.pack ",") xs

-- Extentsion of unpack			 
unPack :: [[T.Text]] -> [[String]]
unPack [] = []
unPack (x:xs) = map T.unpack x : unPack xs

-- Removes empty rows which could be present in the input file
dropEmptyRows, dropIncompRows :: [[String]] -> [[String]]				
dropEmptyRows [] = []				
dropEmptyRows (x:xs) | x == ["","","","","","","","",""] = dropEmptyRows xs
                 | otherwise = x : dropEmptyRows xs

-- Removes rows that could appear due to an incorrect input. Precaution			 
dropIncompRows [] = []
dropIncompRows (x:xs) | isIncomp x = dropIncompRows xs
						| otherwise = x : dropIncompRows xs

-- True if a row doesn't have 9 fields			
isIncomp :: [a] -> Bool						
isIncomp (a:b:c:d:e:f:g:h:i:[]) = False
isIncomp other = True

-- Returns a list of records from a list of list of strings.  Numbers each record also
getRecords :: Int -> [[String]] -> [Record]
getRecords n [] = []
getRecords n (x:xs) = (fromJust $ mgetRecords n x) : getRecords (n+1) xs

-- Determines if a record can be generated correctly
mgetRecords :: Int -> [String] -> Maybe Record
mgetRecords n (a:b:c:d:e:f:g:h:i:[]) = Just (Record n a b c d e f g h i)
mgetRecords n other = Nothing

-- Convert a record to a list
recordToList, recordToList' :: Record -> [String]
recordToList r = [club r,  mapName r, nearestTown r, terrain r, mapGrade r, gridRefOfSWCorner r, gridRefOfNECorner r, expectedCompletionDate r, sizeSqKm r]

recordToList' r = [(show $ row r), club r,  mapName r, nearestTown r, terrain r, mapGrade r, gridRefOfSWCorner r, gridRefOfNECorner r, expectedCompletionDate r, sizeSqKm r]

-- Convert a record to a string, with commas between each field
recordToString :: Record -> String
recordToString r = addC $ intercalate "," $ recordToList r

-- Add a comma to the end of a string. Used as a precaution
addC :: String -> String
addC x = x ++ ","

