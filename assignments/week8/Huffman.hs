
module Huffman
where
import Data.List
import Data.Map(fromList, (!))
import Satellite
import Tree

-- -------------------------------------------------------------------------------

-- Warm-up: constructing a frequency table.

frequencies  ::  (Ord char) => [char] -> [With Int char]
frequencies [] = []
frequencies text = map (\(x,y) -> x :- y) (zip (map length splitList) (map (\seq -> head seq) splitList))
  where splitList = groupBy (\a b -> a == b) $ sort text

-- -------------------------------------------------------------------------------

-- Constructing a Huffman tree.

huffman :: [With Int char] -> Tree char
huffman frequencies = second $ head $ makeHuffmanTree (map (\(a:-b) -> a:-Leaf b) sortedList)
  where sortedList = sort frequencies

makeHuffmanTree :: [With Int (Tree char)] -> [With Int (Tree char)]
makeHuffmanTree pairList
  | length pairList == 1 = pairList
  | otherwise = makeHuffmanTree (sort (buildBranch pairList))

buildBranch :: [With Int (Tree char)] -> [With Int (Tree char)]
buildBranch ((a:-b):(c:-d):xs) = ((a+c):-(b :^: d)) : xs

-- English frequency tree
letterFrequencies = [817, 149, 278, 425, 1270, 223, 202, 609, 697, 15, 77, 403, 241, 675, 751, 193, 10, 599, 633, 906, 276, 98, 236, 15, 197, 7]

-- Create the tree
huffTree = huffman $ sort $ map (\(a, b) -> a:-b) $ zip letterFrequencies ['a'..'z']

-- -------------------------------------------------------------------------------

-- Encoding ASCII text.

data Bit = O | I
  deriving (Show, Eq, Ord)

encode :: (Eq char, Ord char) => Tree char -> [char] -> [Bit]
encode hTree text = concat $ map (\c -> fromList codeList ! c) text
  where codeList = codes hTree

codes :: Tree char -> [(char, [Bit])]
codes tree = buildCodeList tree []

buildCodeList :: Tree char -> [Bit] -> [(char, [Bit])]
buildCodeList (l :^: r) bitStream = (buildCodeList l (bitStream++[O])) ++ (buildCodeList r (bitStream++[I]))
buildCodeList (Leaf elem) bitStream = [(elem, bitStream)]

-- -------------------------------------------------------------------------------

-- Decoding a Huffman binary.

decode :: Tree Char -> [Bit] -> [Char]
decode hTree bitStream
  | ((length (snd (findChar codeList bitStream 1))) >0) = [fst (findChar codeList bitStream 1)] ++ decode hTree (snd (findChar codeList bitStream 1))
  | otherwise = [fst (findChar codeList bitStream 1)]
  where codeList = reverseMap $ codes hTree
  
reverseMap input = map (\(a,b) -> (b,a)) input

--getChar :: [([Bit], char)] -> [Bit] -> (char,[Bit])
--getChar list bitStream = takeWhile (lookup ? list) bitStream

findChar :: [([Bit], Char)] -> [Bit] -> Int -> (Char,[Bit])
findChar codeList bitStream len
  | res == Nothing = findChar codeList bitStream (len+1) 
  | otherwise = (head $ show res, (drop len bitStream))
  where res = (lookup codeList (take len bitStream))



--reverseMap input = map (\(a,b) -> (b,a)) input

-- -------------------------------------------------------------------------------

-- Some test data.

hw, why :: String
hw =
  "hello world"

-- code = huffman (frequencies hw)
-- encode code hw
-- decode code it
-- decode code it == hw

why =
  "As software becomes more and more complex, it\n\
  \is  more  and  more important to structure it\n\
  \well.  Well-structured  software  is  easy to\n\
  \write,   easy   to   debug,  and  provides  a\n\
  \collection  of modules that can be re-used to\n\
  \reduce future programming costs. Conventional\n\
  \languages place a conceptual limit on the way\n\
  \problems   can   be  modularised.  Functional\n\ 
  \languages  push  those  limits  back. In this\n\
  \paper we show that two features of functional\n\
  \languages    in    particular,   higher-order\n\
  \functions and lazy evaluation, can contribute\n\
  \greatly  to  modularity.  Since modularity is\n\
  \the key to successful programming, functional\n\
  \languages  are  vitally important to the real\n\
  \world."

-- code = huffman (frequencies why)
-- encode code why
-- decode code it
-- decode code it == why
