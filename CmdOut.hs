-- Shane Fitzpatrick 09487581
-- Module that returns a record set as a well formatted string
module CmdOut where

import qualified CmdFile as F
import Data.List

printRecords :: [F.Record] -> String
printRecords [] = ""
printRecords r = unlines $ ("\t|Club|\t|MapName|\t|Nearest Town|\t|Terrain|\t|Map Grade|\t|Grid ref of SW corner|\t|Grid ref of NE corner|\t|Expected Completion Date|\t|Size, sq km|" : printRecords' r) 

printRecords' :: [F.Record] -> [String]
printRecords' [] = []
printRecords' (r:rs) = printRecord r : printRecords' rs

printRecord :: F.Record -> String
printRecord r = intercalate "|\t|" $  F.recordToList' r
