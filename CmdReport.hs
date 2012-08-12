-- Shane Fitzpatrick 09487581
-- Module that includes all the logic for the report command
module CmdReport where

import qualified CmdFile as F
import Data.Char
import Data.List
import Data.Maybe
import qualified CmdTrans as Tr
import qualified Utility as U
import Data.Time.Calendar

-- Determines the correct action based on the different possible report commands
parseReport :: Int -> [F.Record] -> String -> String
parseReport _ mState "registrations" = unlines $ sort $ tupleListToStringList $ reverse $ genTupleCount mState []
parseReport date mState "completions" = unlines $ genCompletedList date mState
parseReport _ _ other  = "\n***Incorrect syntax. Type 'help report' for assistance***\n"

----------------------------------------------------------------------------------------------------------------------------
-- REGISTRATIONS

-- Generates a list of tuples, first element being the club name, the second being the amount of maps the club has registered
genTupleCount :: [F.Record] -> [(String, Int)] -> [(String, Int)]
genTupleCount [] tuples = tuples
genTupleCount (x:xs) tuples | mElem (F.club x) (map fst tuples) = genTupleCount xs $ getNewTuples tuples existing
							| otherwise = genTupleCount xs $ (F.club x, 1):tuples
							where 
								existing = getExisting (F.club x) tuples

-- Returns a tuple that has the correct club name								
getExisting :: String -> [(String, Int)] -> (String, Int)
getExisting str (y:ys) | map toLower str == map toLower (fst y) = y
						| otherwise = getExisting str ys

-- Generates the new tuple list with the increment of the appropriate tuple
getNewTuples :: [(String, Int)] -> (String, Int) -> [(String, Int)]
getNewTuples old e = (fst e, 1 + (snd e)):(delete e old)

-- My version of elem that ignores case
mElem :: String -> [String] -> Bool
mElem _ [] = False
mElem str (x:xs) = (map toLower str) == (map toLower x) || mElem str xs
			
-- Takes a list of tuples and converts to a list of strings
tupleListToStringList :: [(String, Int)] -> [String]
tupleListToStringList [] = []
tupleListToStringList (x:xs) = (fst x ++ (", " ++ number)):tupleListToStringList xs
							where 
								number = show $ snd x
								
-----------------------------------------------------------------------------------------------------------------------------
-- COMPLETIONS

-- Returns a list of the maps that are compeleted
genCompletedList :: Int -> [F.Record] -> [String]
genCompletedList _ [] = []
genCompletedList date (x:xs) | isCompleted date (F.expectedCompletionDate x) = F.mapName x : genCompletedList date xs
							 | otherwise = genCompletedList date xs

-- Determines if a map is completed by examining the date on it
isCompleted, oldDate :: Int -> String -> Bool
isCompleted date str = all isSpace str || hasCompleted str || oldDate date str

-- Compares the current date and the expected date of completion. Both in Julian form
oldDate date str = date > (getStrDate str)

-- Determines if a string has the word "completed"
hasCompleted :: String -> Bool
hasCompleted str = isInfixOf "completed" $ map toLower str

-- Function that will convert the expected completed date into the format dd/mm/yyyy and then convert it to Julian form
getStrDate :: String -> Int
getStrDate str | U.isFullDate date = fromIntegral $ toModifiedJulianDay $ fromGregorian (read $ head $ tail $ tail dateSplit) (read $ head $ tail dateSplit) (read $ head dateSplit)
			   | Tr.hasYear str = fromIntegral $ toModifiedJulianDay $ fromGregorian (read year) (12) (31) 			
				where 
						date = Tr.newStandardDate str
						dateSplit = U.splitDate date
						year = head $ filter ((==4).length) (words str)

-----------------------------------------------------------------------------------------------------------------------------