-- Shane Fitzpatrick 09487581
-- Module that list several (possibly unrelated) utility functions that are used 
-- throughout the project by the other modules
module Utility where

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified GlobRegex as Gl
import Text.Regex.Posix ((=~))

-- Function to seperate the user input when coloumns are specified along with data.  
parseColsWithData :: [String] -> [String]
parseColsWithData ws = map (delete ',') $ map trimQuotes $ mergeQuotes $ filter (/= "") $ filter (/= "=") $ splitEquals ws

-- Functions that will return true if the list is in a desired format
validDetails, validSortCond, validSelCond :: [String] -> Bool
validDetails d = length d == 2 && ((head d) `elem` colsS || (head d) `elem` colsN)

validSortCond cond = (length cond > 0) && (all isValidSortCon cond) && (even $ length cond) 

isValidSortCon :: String -> Bool
isValidSortCon c = isCol c || (c `elem` ["ascending","descending"])

validSelCond cond = (length cond > 0) && (all isCol (everyNth 2 ("":cond))) && (even $ length cond)

-- Will not map to lower if the input case is important
mToLowerStr :: String -> String
mToLowerStr str | '"' `elem` str = str
				| otherwise = map toLower str

-- Determines if a string matches the glob string			
hasGlob :: String -> String -> Bool
hasGlob glob str = str =~ (Gl.globToRegex glob)

-- mergeQuotes joins strings that are related via ""
mergeQuotes, mergeQuotes3 :: [String] -> [String]
mergeQuotes [] = []					 
mergeQuotes (x:xs) | odd (length $ filter (=='"') x) = (x ++ ", " ++ mergeQuotes2 xs) : mergeQuotes (mergeQuotes3 xs)
			 | otherwise = x : mergeQuotes xs

-- mergeQuotes2 gets the rest of the string once a quote has been found			 
mergeQuotes2 :: [String] -> String
mergeQuotes2 [] = []			 
mergeQuotes2 (x:xs) | odd (length $ filter (=='"') x) = x
			 | otherwise = (x ++ " " ++ (mergeQuotes2 xs))

-- mergeQuotes3 returns the rest of the list, after the corresponding quote			 
mergeQuotes3 [] = []			 
mergeQuotes3 (x:xs) | odd (length $ filter (=='"') x) = xs
			 | otherwise = mergeQuotes3 xs

-- Function to split all words with an equals in them to two words			 
splitEquals :: [String] -> [String]	
splitEquals ws = map T.unpack $ concatMap (T.splitOn (T.pack "=")) (map T.pack ws)

-- Returns a list of every nth element in the input list
everyNth :: Int -> [a] -> [a]
everyNth n xs = case drop (n-1) xs of
              (y:ys) -> y : everyNth n ys
              [] -> []

-- Determines if the list contains all the coloumns
noMissingCols :: [String] -> Bool			  
noMissingCols cols = any isClub cols && any isMap cols && any isTown cols && any isTerrain cols && any isGrade cols && any isSW cols && any isNE cols && any isCompleted cols && any isSize cols 

-- Restriced characters for windows file names
restrictedChars :: [Char]
restrictedChars = ['/','\\','*','?','<','>','|',':']

-- removes any [] from the list of lists
dropNull :: [[a]] -> [[a]]
dropNull [] = []
dropNull (x:xs) | null x = dropNull xs
				| otherwise = x : dropNull xs
				
-- Overcomes head of []
mHead :: [[a]] -> [a]
mHead [] = []
mHead other = head other

-- Removes excess white space from a string
trimSpace :: String -> String
trimSpace = trimSide . trimSide
    where trimSide = reverse . dropWhile isSpace
	
-- Removes quotes at either end of a string	
trimQuotes :: String -> String	
trimQuotes	= trimQuote . trimQuote
	where trimQuote = reverse . dropWhile (=='"')
	
-- Sets the first character of each word to capital and rest to lower	
capitalize :: String -> String
capitalize str = unwords $ map capital ws
				where ws = words str
	
capital :: String -> String	
capital [] = []
capital (x:xs) = toUpper x: (map toLower xs)				
	
-- Function to avoid Maybe returns from elemIndex			 
getIndex :: [String] -> [String] -> Int
getIndex _ [] = 0
getIndex search (x:xs)  | (map toLower x) `elem` ((map.map) toLower search) = fromJust $ elemIndex (map toLower x) search
						| otherwise = getIndex search xs

-- Determines if the date format has at least 3 fields						
isFullDate :: String -> Bool
isFullDate str = length (map T.unpack $ T.splitOn (T.pack "/") (T.pack str)) == 3		

-- Returns a list of the fields in a date
splitDate :: String -> [String]
splitDate d = 	map T.unpack $ T.splitOn (T.pack "/") (T.pack d)

-- Determines if a date starts with an alphabetic character
startsAlpha :: String -> Bool
startsAlpha (x:xs) = isAlpha x
startsAlpha other = False

-- Returns the head of a string as a single element list
getLeadLetter :: String -> String 
getLeadLetter (x:xs) = x:[]
getLeadLetter [] = []

colsS, colsN :: [String]			  
colsS = ["$1","$2","$3","$4","$5","$6","$7","$8","$9"] 
colsN = ["club","map name","nearest town","terrain","map grade","grid ref of sw corner","grid ref of ne corner","expected completion date","size, sq km"]

-- Determines if a string is a column
isCol :: String -> Bool
isCol x = x `elem` colsS || x `elem` colsN

isClub, isMap, isTown, isTerrain, isGrade, isSW, isNE, isCompleted, isSize :: String -> Bool
isClub s = s=="$1" || (map toLower s)=="club"
isMap s = s=="$2" || (map toLower s)=="map name"
isTown s = s=="$3" || (map toLower s)=="nearest town"
isTerrain s = s=="$4" || (map toLower s)=="terrain"
isGrade s = s=="$5" || (map toLower s)=="map grade"
isSW s = s=="$6" || (map toLower s)=="grid ref of sw corner"
isNE s = s=="$7" || (map toLower s)=="grid ref of ne corner"
isCompleted s = s=="$8" || (map toLower s)=="expected completion date"
isSize s = s=="$9" || (map toLower s)=="size, sq km" 

isValidFormat f = (hasDayC f && hasMonC f && hasYearC f) || hasAllC f

hasMonC str = any (\word -> word `elem` codeList) monCodes
			where codeList = mWords str
			
hasDayC str = any (\word -> word `elem` codeList) dayCodes
			where codeList = mWords str
			
hasYearC str = any (\word -> word `elem` codeList) yearCodes
			where codeList = mWords str
			
hasAllC str = any (\word -> word `elem` codeList) allCodes
			where codeList = mWords str

mWords str = map T.unpack $ concatMap (T.splitOn (T.pack "/")) (mWords' $ map T.pack $ words str) 

mWords' str = concatMap (T.splitOn (T.pack "-")) str	

dayCodes, monCodes, yearCodes, allCodes :: [String]
dayCodes = ["%d","%e","%j"]
monCodes = ["%B","%b","%h","%m"]
yearCodes = ["%Y","%y","%c","%G","%g","%f"]
allCodes = ["%D","%F","%x"]

-- Mathes the column number to the name
convertToColName :: String -> String
convertToColName "$1" = "club"
convertToColName "$2" = "map name"
convertToColName "$3" = "nearest town"
convertToColName "$4" = "terrain"
convertToColName "$5" = "map grade"
convertToColName "$6" = "grid ref of sw corner"
convertToColName "$7" = "grid ref of ne corner"
convertToColName "$8" = "expected completion date"
convertToColName "$9" = "size sq km"
convertToColName other = other