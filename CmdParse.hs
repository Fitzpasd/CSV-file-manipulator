-- Shane Fitzpatrick 09487581
-- Module that will parse the input string and determine the correct function to call
module CmdParse where

import Data.Char
import qualified CmdHelp as H
import qualified CmdFile as F
import qualified CmdReport as R
import qualified Utility as U
import qualified CmdTrans as T
import qualified CmdEdit as E
import qualified CmdGen as G
import qualified CmdOut as O

parse :: [String] -> [F.Record] -> [F.Record] -> String
parse ("count":xs) records _ = (show $ length $ snd $ E.select (U.parseColsWithData xs) records) ++ " value(s) for " ++ (unwords xs)
parse ("list":xs) records _ = O.printRecords $ snd $ E.select (U.parseColsWithData xs) records
parse ("distinct":col:xs) records _ = G.distinct col records
parse ("show":xs) _ selected = O.printRecords selected 
parse ("help":xs) records _ = H.parseHelp $ U.mHead xs
parse other _ _ = "\n***Not a valid command***\n"

parseR :: [String] -> [F.Record] -> Int -> String
parseR ("report":xs) records date = R.parseReport date records $ U.mHead xs

-- Determines if the command changes state
changeState, changeIO, isLoad :: String -> Bool
changeState "date-fix" = True
changeState "grid-fix" = True
changeState "reformat" = True
changeState "sort" = True
changeState "update" = True
changeState "delete" = True
changeState "insert" = True
changeState other = False

parseCS :: [String] -> [F.Record] -> (String, [F.Record])
parseCS ("date-fix":xs) records = T.fixDates records (U.mHead $ U.parseColsWithData xs) (U.mHead $ tail $ U.parseColsWithData xs)
parseCS ("grid-fix":xs) records = T.fixGrids records (U.mHead $ U.parseColsWithData xs) (U.mHead $ tail $ U.parseColsWithData xs)
parseCS ("reformat":xs) records = T.reformat (U.mHead $ U.parseColsWithData xs) (U.mHead $ tail $ U.parseColsWithData xs) records
parseCS ("sort":xs) records = E.mSort (U.parseColsWithData xs) records
parseCS ("update":row:xs) records = E.updateRow (read row) (U.parseColsWithData xs) records
parseCS ("delete":row:xs) records = E.deleteRow (read row) records
parseCS ("insert":xs) records = E.insertRow (U.parseColsWithData xs) records
parseCS _ records  = ("Incorrect command that changes state", records)

parseSel :: [F.Record] -> [String] -> (String, [F.Record])
parseSel records ("all":xs) = ("All records selected", records)
parseSel records x = E.select (U.parseColsWithData x) records

changeIO "load" = True
changeIO "save" = True
changeIO other = False

isLoad "load" = True
isLoad other = False

-- Returns the head of a string list without the quotes
getFile :: [String] -> String
getFile [] = []
getFile (x:xs) = U.trimQuotes x

-- Determines if a string is a valid filename
isValidFile :: String -> Bool
isValidFile [] = False
isValidFile str = endCSV (reverse str) && onlyChars str

-- Determines if a file ends with .csv
endCSV :: String -> Bool
endCSV (x:y:z:a:xs) = x=='v' && y=='s' && z=='c' && a=='.' 

-- Determines if a string contains only legal characters
onlyChars :: String -> Bool
onlyChars [] = True
onlyChars str = and $ map myChars str

-- Determines if a character is a legal character to use in a file name
myChars :: Char -> Bool
myChars c = isAlphaNum c || (not $ elem c U.restrictedChars)