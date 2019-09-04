-- 1.1
-- 1: Returns 6
-- 2: Returns min(m, n)
-- 3: returns the entered text n times
-- 4: performs the mod operator on x and y and then on y (x mod y)
-- 5: adds the first element of the list to the second element
-- 6: inverts the list
-- 7: inverts the list twice
-- 1.2
-- The part between the curly brackets are the ones that are rewritten
-- e1 = {42} = 42
-- e2 = 1 + 125 * 8 / 10 - 59
-- e2 = 1 + 125 * {8 / 10} - 59
-- e2 = 1 + {125 * 0.8} - 59
-- e2 = 1 + {100 - 59}
-- e2 = {1 + 41}
-- e2 = 42
-- e3 = not True || True && False
-- e3 = not True || {True && False}
-- e3 = {not True} || False
-- e3 = {False || False}
-- e3 = False
-- e4 = 1 + 2 == 6 - 3
-- e4 = 1 + 2 == {6 - 3}
-- e4 = {1 + 2} == 3
-- e4 = {3 = 3}
-- e4 = True
-- e5 = "1 + 2" == "6 - 3"
-- e5 = {"1 + 2" == "6 - 3"}
-- e5 = False
-- e6 = "1111 + 2222" == "1111" ++ " + " ++ "2222"
-- e6 = "1111 + 2222" == "1111" ++ {" + " ++ "2222"}
-- e6 = "1111 + 2222" == {"1111" ++ " + 2222"}
-- e6 = {"1111 + 2222" == "1111 + 2222"}
-- e6 = True
-- 1.3
-- See Database.hs
-- 1.4
-- 1.5
-- 1.6
-- 1.7
thisOldMan :: String
thisOldMan = printVerses "" 0

printVerses :: String->Int->String
printVerses s n
  | n >= 10 = s
  | otherwise = printVerse s n ++ printVerses s (n+1)

printVerse :: String->Int->String
printVerse s n = s ++ head poemSentenceNumber ++ head (poemValues !! n) ++ last poemSentenceNumber ++ head poemSentenceThing ++ last (poemValues !! n) ++ last poemSentenceThing ++ poemEnd

poemSentenceNumber :: [String]
poemSentenceNumber = ["This old man, he played ", ",\n"]
poemSentenceThing :: [String]
poemSentenceThing = ["He played knick-knack on my ", ";\n"]

poemEnd :: String
poemEnd = "With a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home.\n\n"
poemValues :: [[String]]
poemValues = [["one", "thumb"], ["two", "shoe"], ["three", "knee"], ["four", "door"], ["five", "hive"], ["six", "sticks"], ["seven", "heaven"], ["eight", "gate"], ["nine", "spine"], ["ten", "again"]]
