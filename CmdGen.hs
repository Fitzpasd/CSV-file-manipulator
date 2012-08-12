-- Shane Fitzpatrick 09487581
-- Module that describes logic for general commands
module CmdGen where

import qualified Utility as U
import qualified CmdFile as F
import Data.List

-------------------------------------------------------------------------------------------------------------------------
-- Distinct

-- Returns a string with the appropriate data.  Counts the distinct values by removing the duplicates in a list of the column values and getting the length
distinct :: String -> [F.Record] -> String
distinct col records | U.isClub col = (show $ length $ nub $ map F.club records) ++ " distinct values for " ++ U.convertToColName col
					| U.isMap col = (show $ length $ nub $ map F.mapName records) ++ " distinct values for " ++ U.convertToColName col
					| U.isTown col = (show $ length $ nub $ map F.nearestTown records) ++ " distinct values for " ++ U.convertToColName col
					| U.isTerrain col = (show $ length $ nub $ map F.terrain records) ++ " distinct values for " ++ U.convertToColName col
					| U.isGrade col = (show $ length $ nub $ map F.mapGrade records) ++ " distinct values for " ++ U.convertToColName col
					| U.isSW col = (show $ length $ nub $ map F.gridRefOfSWCorner records) ++ " distinct values for " ++ U.convertToColName col
					| U.isNE col = (show $ length $ nub $ map F.gridRefOfNECorner records) ++ " distinct values for " ++ U.convertToColName col
					| U.isCompleted col = (show $ length $ nub $ map F.expectedCompletionDate records) ++ " distinct values for " ++ U.convertToColName col
					| U.isSize col = (show $ length $ nub $ map F.sizeSqKm records) ++ " distinct values for " ++ U.convertToColName col
					| otherwise = "Column does not exist"