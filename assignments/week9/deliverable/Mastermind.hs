module Mastermind
where
import System.Random
import Data.Char
import Data.List
import Control.Monad

data Colors = White | Silver | Green | Red | Orange | Pink | Blue | Yellow deriving (Eq,Enum,Show)

convertToColor :: String -> Colors
convertToColor str
  | s == "white"  = White
  | s == "silver" = Silver
  | s == "green"  = Green
  | s == "red"    = Red
  | s == "orange" = Orange
  | s == "pink"   = Pink
  | s == "blue"   = Blue
  | s == "yellow" = Yellow
  | otherwise = error "Unknown color"
  where s = map toLower str

convertToString :: Colors -> String
convertToString c
  | c == White  = "white" 
  | c == Silver = "silver"  
  | c == Green  = "green" 
  | c == Red    = "red"   
  | c == Orange = "orange"
  | c == Pink   = "pink"  
  | c == Blue   = "blue"  
  | c == Yellow = "yellow"
  | otherwise = error "Unknown color"

-- Run using masterMind 8 12 4
masterMind :: Int -> Int -> Int -> IO ()
masterMind nColors nMaxAttempts sequenceLength = do
  codeWord <- getCodeWord nColors sequenceLength
  makeAttempt codeWord nMaxAttempts

getCodeWord :: Int -> Int -> IO [Int]
getCodeWord nColors sequenceLength = do
  let randomColor = randomRIO (0, nColors - 1) -- IO a
  let codeWord = replicate sequenceLength randomColor -- IO [Int]
  sequence codeWord

makeAttempt :: [Int] -> Int -> IO ()
makeAttempt codeWord nRemainingAttempts =
  if (nRemainingAttempts == 0) 
  then
    do
    putStrLn "No attempts left, you failed to find the codeWord."
    putStrLn ("The keyword was : " ++ show (map convertToString [toEnum c::Colors | c <- codeWord]))
  else
    do
    putStrLn ("There are " ++ show nRemainingAttempts ++ " attempt(s) left.")
    putStrLn "Guess the codeWord."
    putStr "Guess : "
    inputColors <- getLine
    let colors = map convertToColor (words inputColors)
    if (length codeWord) /= (length colors) 
    then 
      do
      putStrLn "The number of colors does not equal the number of colors in the code"
      putStrLn ("Try again, with " ++ show (length codeWord) ++ " colors.")
      makeAttempt codeWord nRemainingAttempts
    else
      if codeWord == (map fromEnum colors)
      then
        putStrLn "You found the code! Congratulations!"
      else
        do
        giveFeedback codeWord (map fromEnum colors) -- [Int] [Int]
        makeAttempt codeWord (nRemainingAttempts - 1)

giveFeedback :: [Int] -> [Int] -> IO()
giveFeedback codeWord colors = do
  let nCorrectColorPos = length $ filter (== True) (map (\(x,y) -> x == y) (zip codeWord colors)) --Correct colors in right positions
  putStrLn ("Number of correct colors in the right position : " ++ show nCorrectColorPos)
  let remainder = unzip $ filter (\(x,y) -> x /= y) (zip codeWord colors) -- Filter the found values from the list 
  let nCorrectColorWrongPos = findNumberOfWrongPosColors (fst remainder) (snd remainder)
  putStrLn ("Number of correct colors in the wrong position : " ++ show nCorrectColorWrongPos)


findNumberOfWrongPosColors :: [Int] -> [Int] -> Int
findNumberOfWrongPosColors [] _ = 0
findNumberOfWrongPosColors (x:xs) guessedColors = 
  (if (any (== x) guessedColors) then 1 else 0) + (findNumberOfWrongPosColors xs (delete x guessedColors))
