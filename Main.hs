-- Shane Fitzpatrick 09487581
-- Module that holds all IO functions
module Main where

import Data.Char
import Data.Time.Clock
import Data.Time.Calendar
import System.IO.Unsafe
import qualified CmdParse as P
import qualified CmdFile as F
import qualified Utility as U

-- Calls the run1 function with the initial data
run :: IO()
run = run1 (True, []) []

-- Function that is called recursivly until the user opts to quit.
-- Calls the loopF function which will interpret the input
run1 :: (Bool, [F.Record]) -> [F.Record] -> IO()
run1 mState selected = do
						putStr "> "
						input <- getLine
						let i = map U.mToLowerStr $ words input
						loopF i mState selected
				
-- Interprets the input and calls the appropriate functions
loopF :: [String] -> (Bool, [F.Record]) -> [F.Record] -> IO()
loopF input mState selected | U.mHead input == "quit" && fst mState = putStrLn "\n***Exiting the program.***\n"	
							| U.mHead input == "quit" = do 
														 putStrLn "Unsaved record set, would you like to save before quitting? (y/n) \n"
														 answer <- getChar
														 if answer == 'n'
															then run1 mState selected
														 else quitWithSave (snd mState)	
							
							| U.mHead input == "select" = do
															let newSel = P.parseSel (snd mState) $ tail input
															putStrLn (fst newSel)
															run1 mState (snd newSel)
															
							| P.changeIO $ U.mHead input = do
														let fileName = P.getFile $ tail input
														if P.isLoad $ U.mHead input
															then do
																if P.isValidFile fileName
																then do 
																	file <- readFile $ P.getFile $ tail input
																	let records = F.load file
																	let numRecords = F.row $ head $ reverse records
																	let output = "1 header file (9 named fields), " ++ (show numRecords) ++ " records"
																	putStrLn output
																	putStrLn "Skipped all blank records"
																	run1 (True, records) [] 
																	else do
																	putStrLn "Not a valid filename."	
																	run1 mState selected
																
															else do
																let newFile = map F.recordToString $ snd mState
																if P.isValidFile fileName
																then do
																	writeFile fileName "Club,Map Name,Nearest Town,Terrain,Map grade,Grid ref of SW corner,Grid ref of NE corner,Expected Completion Date,\"Size, sq km\",\n"
																	appendFile fileName $ unlines newFile 
																	run1 (True, snd mState) selected
																else do
																	putStrLn "Not a valid filename."
																	run1 mState selected
																				
							| P.changeState $ U.mHead input = do
														let newTuple = P.parseCS input $ snd mState
														putStrLn $ fst newTuple
														run1 (False, snd $ newTuple) selected
							| U.mHead input == "report" = do
															putStrLn $ P.parseR input (snd mState) getCurrentDate	
															run1 mState selected
							| otherwise = do
											putStrLn $ P.parse input (snd mState) selected	
											run1 mState selected

-- Function that will first save the record set to an untitled csv file and then quit
quitWithSave :: [F.Record] -> IO()									
quitWithSave records = do
						let newFile = map F.recordToString records
						let fileName = "Untitled.csv"
						writeFile fileName "Club,Map Name,Nearest Town,Terrain,Map grade,Grid ref of SW corner,Grid ref of NE corner,Expected Completion Date,\"Size, sq km\",\n"
						appendFile fileName $ unlines newFile 
						putStrLn "\n***Exiting the program.***\n"

						
-- Function that will determine the current date.  Unsafe IO is performed here only.
getCurrentDate :: Int
getCurrentDate = fromIntegral $ unsafePerformIO getDate	

getDate :: IO Integer
getDate = getCurrentTime >>= return . toModifiedJulianDay . utctDay							