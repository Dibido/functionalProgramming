module Assignments where
import Prelude hiding (Word)
import Data.List
lorem = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diamnonumy eirmod tempor invidunt ut labore et dolore magna aliquyamerat, sed diam voluptua. At vero eos et accusam et justo duo doloreset ea rebum. Stet clita kasd gubergren, no sea takimata sanctus estLorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetursadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore etdolore magna aliquyam erat, sed diam voluptua. At vero eos et accusamet justo duo dolores et ea rebum. Stet clita kasd gubergren, no seatakimata sanctus est Lorem ipsum dolor sit amet."
--3.1
type Word = String
wordList :: String -> [(Word, Int)]
--wordList s = [("ba",2)]
wordList s = let values = group $ sort $ words $ lorem [((map $ length $ values), (map $ head $ values))
