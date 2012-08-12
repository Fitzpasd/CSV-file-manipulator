-- Shane Fitzpatrick 09487581
-- Module that describes the logic for the commands that edit the loaded record set
module CmdEdit where

import qualified CmdFile as F
import qualified Data.Text as T
import qualified Utility as U
import Data.List
import Data.Maybe

-------------------------------------------------------------------------------------------------------------------------
-- Select

-- Returns the tuple of feedback and new selected record set
select :: [String] -> [F.Record] -> (String, [F.Record])
select conds records | U.validSelCond conds = ((show $ length newSel) ++ " records selected", newSel) 
					| otherwise = ("Invalid conditions. Consult help", records)
						where newSel = getNewSel conds records

-- Returns a new seleced record set based on the conditions and the full record set						
getNewSel :: [String] -> [F.Record] -> [F.Record]
getNewSel _ [] = []
getNewSel conds (x:xs) | satisfyConds conds x = x : getNewSel conds xs
					   | otherwise = getNewSel conds xs

-- Determines if a record satisfies all the conditions					   
satisfyConds :: [String] -> F.Record -> Bool
satisfyConds [] _ = True
satisfyConds (col:glob:xs) r | satisfyCond col glob r = satisfyConds xs r
							 | otherwise = False

-- Determines if a record satisfies one condition							 
satisfyCond :: String -> String -> F.Record -> Bool
satisfyCond col glob r 	| U.isClub col = U.hasGlob glob $ F.club r
						| U.isMap col = U.hasGlob glob $ F.mapName r
						| U.isTown col = U.hasGlob glob $ F.nearestTown r
						| U.isTerrain col = U.hasGlob glob $ F.terrain r
						| U.isGrade col = U.hasGlob glob $ F.mapGrade r
						| U.isSW col = U.hasGlob glob $ F.gridRefOfSWCorner r
						| U.isNE col = U.hasGlob glob $ F.gridRefOfNECorner r
						| U.isCompleted col =U.hasGlob glob $ F.expectedCompletionDate r
						| U.isSize col = U.hasGlob glob $ F.sizeSqKm r
						| otherwise = False

-------------------------------------------------------------------------------------------------------------------------
-- Delete

-- Returns the record set without the row if it is in the set
deleteRow :: Int -> [F.Record] -> (String, [F.Record])
deleteRow row records | row `elem` (map F.row records) = ("Row deleted", newRecords)
					  | otherwise = ("Delete failed - row does not exist", records)
						where newRecords = delete (getRecord row records) records

-- Returns a record matching the row number. Returns Null if row isnt in set
getRecord :: Int -> [F.Record] -> F.Record
getRecord _ [] = F.Null
getRecord row (x:xs) | F.row x == row = x
					 | otherwise = getRecord row xs
					 
-------------------------------------------------------------------------------------------------------------------------
-- Insert
-- Colss here too
-- Returns the tuple with feedback and new record set
insertRow :: [String] -> [F.Record] -> (String, [F.Record])
insertRow details records 	| all U.isCol colsIn && U.noMissingCols colsIn = ("Row inserted at position " ++ (show $ getNewRowNum records), reverse (newRecord : reverse records))
							| otherwise = ("Error - incorrect format or the following rows are missing:\n" ++ (getMissingRows colsIn), records)
								where 
									colsIn = U.everyNth 2 ("":details)
									dataIn = U.everyNth 2 details
									newRecord = fromJust $ F.mgetRecords (getNewRowNum records) dataIn

-- Returns a string of rows that are missing from the input	
-- **********************************								
getMissingRows :: [String] -> String
getMissingRows details = unlines $ nub $ map U.convertToColName ((U.colsS `union` U.colsN) \\ details)

-- Returns the new row number of the row being inserted.  Is the last row number used plus 1
getNewRowNum :: [F.Record] -> Int
getNewRowNum [] = 1
getNewRowNum r = 1 + (F.row $ head $ reverse r)
			  
-------------------------------------------------------------------------------------------------------------------------
-- Update

-- Returns the tuple with Feedback and the new recordset
updateRow :: Int -> [String] -> [F.Record] -> (String, [F.Record])
updateRow rowNum details records | U.validDetails details && rowNum `elem` (map F.row records) = ("Row updated",  (updateRow' r details) : (snd $ deleteRow rowNum records))
								 | not $ rowNum `elem` (map F.row records) = ("Row does not exist", records)
								 | otherwise = ("Incorrect column details combination", records)
								where 
									r = getRecord rowNum records

-- Returns a new row with the same cells other than the one to update									
updateRow' :: F.Record -> [String] -> F.Record
updateRow' r (x:y:xs) | U.isClub x = F.Record (F.row r) y (F.mapName r) (F.nearestTown r) (F.terrain r) (F.mapGrade r) (F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (F.expectedCompletionDate r) (F.sizeSqKm r)
					| U.isMap x = F.Record (F.row r) (F.club r) y (F.nearestTown r) (F.terrain r) (F.mapGrade r) (F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (F.expectedCompletionDate r) (F.sizeSqKm r)
					| U.isTown x = F.Record (F.row r) (F.club r) (F.mapName r) y (F.terrain r) (F.mapGrade r) (F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (F.expectedCompletionDate r) (F.sizeSqKm r)
					| U.isTerrain x = F.Record (F.row r) (F.club r) (F.mapName r) (F.nearestTown r) y (F.mapGrade r) (F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (F.expectedCompletionDate r) (F.sizeSqKm r)
					| U.isGrade x = F.Record (F.row r) (F.club r) (F.mapName r) (F.nearestTown r) (F.terrain r) y (F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (F.expectedCompletionDate r) (F.sizeSqKm r)
					| U.isSW x = F.Record (F.row r) (F.club r) (F.mapName r) (F.nearestTown r) (F.terrain r) (F.mapGrade r) y (F.gridRefOfNECorner r) (F.expectedCompletionDate r) (F.sizeSqKm r)
					| U.isNE x = F.Record (F.row r) (F.club r) (F.mapName r) (F.nearestTown r) (F.terrain r) (F.mapGrade r) (F.gridRefOfSWCorner r) y (F.expectedCompletionDate r) (F.sizeSqKm r)
					| U.isCompleted x = F.Record (F.row r) (F.club r) (F.mapName r) (F.nearestTown r) (F.terrain r) (F.mapGrade r) (F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) y (F.sizeSqKm r)
					| U.isSize x = F.Record (F.row r) (F.club r) (F.mapName r) (F.nearestTown r) (F.terrain r) (F.mapGrade r) (F.gridRefOfSWCorner r) (F.gridRefOfNECorner r) (F.expectedCompletionDate r) y
					| otherwise = r	

-------------------------------------------------------------------------------------------------------------------------
-- Sort

mSort :: [String] -> [F.Record] -> (String, [F.Record])
mSort cond records 	| U.validSortCond cond = ("All records sorted", newRecords)
					| otherwise = ("Sort conditions are invalid. Consult help", records)
						where newRecords = mQsort cond records
						
mQsort :: [String] -> [F.Record] -> [F.Record]
mQsort _ [] = []
mQsort [] r = r
mQsort c@(col:cond:cs) l@(x:xs) = (mQsort c small) ++ mid ++ (mQsort c large)
   where
    small = [y | y<-xs, (qGetCond cond) (qGetStr col y) (qGetStr col x)]
    mid   = mQsort cs [y | y<-l, (qGetStr col y) == (qGetStr col x)]
    large = [y | y<-xs,  (qGetCond' cond) (qGetStr col y) (qGetStr col x)]
	
qGetStr :: String -> F.Record -> String
qGetStr col r | U.isClub col = F.club r
			| U.isMap col = F.mapName r
			| U.isTown col = F.nearestTown r
			| U.isTerrain col = F.terrain r
			| U.isGrade col =F.mapGrade r
			| U.isSW col = F.gridRefOfSWCorner r
			| U.isNE col = F.gridRefOfNECorner r
			| U.isCompleted col = F.expectedCompletionDate r
			| U.isSize col = F.sizeSqKm r
			| otherwise = ""

qGetCond "ascending" = (<)
qGetCond "descending" = (>)
qGetCond' "ascending" = (>)
qGetCond' "descending" = (<)			

	