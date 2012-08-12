-- Shane Fitzpatrick 09487581
-- Module that includes the logic for the commands that are transformations
module CmdTrans where 

import qualified CmdFile as F
import qualified Utility as U
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Time.Format
import Data.Time.Clock.POSIX
import Locale

-- Determines the amount of different recrods between two sets
numAlterations :: [F.Record] -> [F.Record] -> Int
numAlterations records1 records2 = length $ records1 \\ records2

-------------------------------------------------------------------------------------------------------------------------
-- Date-fix

-- Function that takes the coloumn, format and record set and returns a tuple with the feedback and new record set
fixDates :: [F.Record] -> String -> String -> (String, [F.Record])
fixDates records col format | U.isCompleted col && U.isValidFormat format = ((show $ numAlterations records newRecords) ++ " records adjusted", newRecords)
							| otherwise = ("Error with command. Please consult help", records)
								where newRecords = map (newDateRecord format) records 

newDateRecord :: String -> F.Record -> F.Record								
newDateRecord format r  | standardDate /= (F.expectedCompletionDate r) = F.Record (F.row r) (F.club r) (F.mapName r) (F.nearestTown r) (F.terrain r) (F.mapGrade r) (F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (formatTime defaultTimeLocale format (getDay standardDate)) (F.sizeSqKm r)
						| otherwise = r
							where standardDate = newStandardDate (F.expectedCompletionDate r)

-- Returns the a Day representing by the str						
getDay :: String -> Day
getDay str = readTime defaultTimeLocale "%d/%m/%Y" str

-- Functions that return a new date based on the old and format
-- Standard date = dd/mm/yyyy
newStandardDate, newDateWithStr, getNewYearDate :: String -> String
newStandardDate old | old == "" = ""
					| (length $ words old) > 1 = newDateWithStr old
					| '-' `elem` old = getNewYearDate old
					| elem '/' old = (getDayMon ws) ++ "/" ++ year
					| otherwise = old
						where 
							ws = map T.unpack $ T.splitOn (T.pack "/") (T.pack old)
							year = getYear $ intercalate " " ws
							
newDateWithStr old 	| isAcceptableStr old = "01/" ++ (getMonthW old) ++ (getYear old)
					| otherwise = old
							
getNewYearDate old = day ++ "/" ++ month ++ "/2012"
					where 
						ws = map T.unpack $ T.splitOn (T.pack "-") (T.pack old)
						day = addZero $ head $ filter (all isDigit) ws
						month = addZero $ show (1 + (U.getIndex abbMons ws)) 

-- getMonthW = written
getMonthW, getYear, mRotate, addZero :: String -> String
-- Returns the integer relating to the month that is present in the passed string (str)
getMonthW str | hasMonth str = (addZero $ show (1 + (U.getIndex fullMons ws))) ++ "/"
			 | hasAbbMon str = (addZero $ show (1 + (U.getIndex abbMons ws))) ++ "/"
			 | hasSeason str = (addZero $ show $ getSeasonMon $  fromJust $ find (\season -> season `elem` seasons) ws) ++ "/"
			 | otherwise = "01/"
				where ws = words str

-- Returns the year that is in the passed string.  Assumes that and 4 digit number is the year				
getYear str | hasYear str = fromJust $ find (\ws -> ( and $ map isDigit ws) && (length ws == 4)) ws
			| otherwise = "0"
				where ws = words str

-- Rotates a string around the '/'. Example - "01/12" -> "12/01"				
mRotate str = (head $ tail ws) ++ "/" ++ (head ws)
             where ws = map T.unpack $ T.splitOn (T.pack "/") (T.pack str)
	
-- Adds a 0 to a string that is only one character long	
addZero str | length str == 1 = "0" ++ str
			| otherwise = str

-- Return the int relating to the month that is at the start of the season			
getSeasonMon :: String -> Int
getSeasonMon "spring" = 3
getSeasonMon "summer" = 5
getSeasonMon "autumn" = 8
getSeasonMon "winter" = 11

-- Returns the day and month from a list in the form dd/mm
getDayMon :: [String] -> String
getDayMon ws | length ws == 1 = "01/01"
			 | length ws == 2 = "01/" ++ (addZero $ fromJust $ find (\ws -> (and $ map isDigit ws) && (length ws ==2 )) ws) --ASSUMING THE YEAR ISNT FIRST!!!
			 | length ws == 3 && dayEqMon dayAndMon = (addZero $ head dayAndMon) ++ "/" ++ (addZero $ head dayAndMon)  
			 | length ws == 3 && (read $ head dayAndMon) > 12 = (addZero $ head $ tail dayAndMon) ++ "/" ++ (addZero $ head dayAndMon) 
			 | length ws == 3 && (read $ head $ tail dayAndMon) > 12 = (addZero $ head dayAndMon) ++ "/" ++ (addZero $ head $ tail dayAndMon)
			 | length ws == 3 = (head dayAndMon) ++ "/" ++ (addZero $ head $ tail dayAndMon) 
			 | otherwise = "SOMETHING HAS GONE WRONG - too many values passed to CmdTrans.getDayMon"
				where dayAndMon = filter ((<=2).length) ws

-- Determines if the day and month integer values are equal				
dayEqMon :: [String] -> Bool
dayEqMon (x:y:xs) = x == y
dayEqMon other = False			 

-- Functions to detemine if the string can be changed to the appropriate date
isAcceptableStr, hasMonth, hasFullMon, hasAbbMon, hasSeason, hasYear :: String -> Bool
isAcceptableStr str = hasYear str && (hasMonth str || hasSeason str)

hasMonth str = hasFullMon str || hasAbbMon str

hasFullMon str = any (\word -> word `elem` ws) fullMons
  where ws = words str
	
hasAbbMon str = any (\word -> word `elem` ws) abbMons
  where ws = words str
  
hasSeason str = any (\word -> word `elem` ws) seasons
  where ws = words str 

hasYear str =  or $ map and $ map (map isDigit) $ filter (\ws -> length ws == 4) ws
  where ws = words str

fullMons, abbMons, seasons :: [String] 
fullMons = ["january", "febuary", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"]	
abbMons = ["jan","feb","mar","apr","may","jun","jul","aug","sept","oct","nov","dec"]
seasons = ["spring","summer","autumn","winter"]

-------------------------------------------------------------------------------------------------------------------------
-- Reformat

-- Returns the tuple with new record set and feedback
reformat :: String -> String -> [F.Record] -> (String, [F.Record])
reformat col method records = ((show $ numAlterations records newRecords)++ " records adjusted", newRecords)
							where newRecords = map (newReformatRecord col method) records

-- Creates a new record based on the previous record, column to change and method of reformatting
newReformatRecord :: String -> String -> F.Record -> F.Record
newReformatRecord col method r	| U.isClub col = F.Record (F.row r) (function $ F.club r) (F.mapName r) (F.nearestTown r) (F.terrain r) (F.mapGrade r) (F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (F.expectedCompletionDate r) (F.sizeSqKm r)
								| U.isMap col = F.Record (F.row r) (F.club r) (function $ F.mapName r) (F.nearestTown r) (F.terrain r) (F.mapGrade r) (F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (F.expectedCompletionDate r) (F.sizeSqKm r)
								| U.isTown col = F.Record (F.row r) (F.club r) (F.mapName r) (function $ F.nearestTown r) (F.terrain r) (F.mapGrade r) (F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (F.expectedCompletionDate r) (F.sizeSqKm r)
								| U.isTerrain col = F.Record (F.row r) (F.club r) (F.mapName r) (F.nearestTown r) (function $ F.terrain r) (F.mapGrade r) (F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (F.expectedCompletionDate r) (F.sizeSqKm r)
								| U.isGrade col = F.Record (F.row r) (F.club r) (F.mapName r) (F.nearestTown r) (F.terrain r) (function $ F.mapGrade r) (F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (F.expectedCompletionDate r) (F.sizeSqKm r)
								| U.isSW col = F.Record (F.row r) (F.club r) (F.mapName r) (F.nearestTown r) (F.terrain r) (F.mapGrade r) (function $ F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (F.expectedCompletionDate r) (F.sizeSqKm r)
								| U.isNE col = F.Record (F.row r) (F.club r) (F.mapName r) (F.nearestTown r) (F.terrain r) (F.mapGrade r) (F.gridRefOfSWCorner r) (function $ F.gridRefOfNECorner r) (F.expectedCompletionDate r) (F.sizeSqKm r)
								| U.isCompleted col = F.Record (F.row r) (F.club r) (F.mapName r) (F.nearestTown r) (F.terrain r) (F.mapGrade r) (F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (function $ F.expectedCompletionDate r) (F.sizeSqKm r)
								| U.isSize col = F.Record (F.row r) (F.club r) (F.mapName r) (F.nearestTown r) (F.terrain r) (F.mapGrade r) (F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (F.expectedCompletionDate r) (function $ F.sizeSqKm r)
								| otherwise = r
										where function = getFunct method

-- Returns the appropriate function based on the user input										
getFunct :: String -> String -> String
getFunct "uppercase" = map toUpper
getFunct "lowercase" = map toLower
getFunct "capitalize" = U.capitalize
getFunct "trim" = U.trimSpace
							
-------------------------------------------------------------------------------------------------------------------------
-- 	Grid-Fix

-- Returns the new tuple with feedback and the record set with fixed grids
fixGrids :: [F.Record] -> String -> String -> (String, [F.Record])
fixGrids records col format | U.isCol col && (format `elem` ["4", "6"]) = ((show $ numAlterations records newRecords)++ " records adjusted", newRecords)
							| otherwise = ("Incorrect grid-fix command", records)
								where newRecords = map (newGridRecord col format) records

-- Returns a new record with its grid fixed based on the column and format								
newGridRecord :: String -> String -> F.Record -> F.Record
newGridRecord col format r 	| U.isSW col = F.Record (F.row r) (F.club r) (F.mapName r) (F.nearestTown r) (F.terrain r) (F.mapGrade r) (gridF $ F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (F.expectedCompletionDate r) (F.sizeSqKm r)
							| U.isNE col = F.Record (F.row r) (F.club r) (F.mapName r) (F.nearestTown r) (F.terrain r) (F.mapGrade r) (F.gridRefOfSWCorner r) (gridF $ F.gridRefOfNECorner r) (F.expectedCompletionDate r) (F.sizeSqKm r)
							| otherwise = r
								where gridF = gridChange format

-- Changes a grid based on format and previous value
gridChange :: String -> String -> String
gridChange format grid 	| format=="4" = intercalate ", " $ map convertTo4 ws
						| format=="6" = intercalate ", " $ map convertTo6 ws
						| otherwise = grid
							where ws = words $ delete ',' grid
							
convertTo4, convertTo4', convertTo6, convertTo6' :: String -> String
convertTo4 grid | U.startsAlpha grid = (U.getLeadLetter grid) ++ (convertTo4' $ filter isDigit grid)
				| otherwise = convertTo4' $ filter isDigit grid

convertTo4' grid | length grid == 3 = grid ++ "0"
				 | length grid == 4 = grid
				 | length grid == 5 = (take 2 grid) ++ (reverse $ take 2 $ reverse grid)
				 | length grid == 6 = (take 2 grid) ++ (reverse $ take 2 $ reverse grid)
				 | otherwise = grid
				 
convertTo6 grid | U.startsAlpha grid = (U.getLeadLetter grid) ++ (convertTo6' $ filter isDigit grid)
				| otherwise = convertTo6' $ filter isDigit grid

convertTo6' grid | length grid == 3 = grid ++ "000"
				 | length grid == 4 = (take 2 grid) ++ "0" ++ (reverse $ take 2 $ reverse grid) ++ "0"
				 | length grid == 5 = grid ++ "0"
				 | length grid == 6 = grid
				 | otherwise = grid