module DNA
where
import Prelude hiding (filter)
import Data.List hiding (filter)
import List

-- Nucleobases or DNA-bases are the basic building blocks of
-- deoxyribonucleic acid (DNA).

data Base  =  A | C | G | T
  deriving (Eq, Ord)

-- Adenin (A), Cytosin (C), Guanin (G) und Thymin (T).

instance Show Base where
  showsPrec _ A  =  showChar 'A'
  showsPrec _ C  =  showChar 'C'
  showsPrec _ G  =  showChar 'G'
  showsPrec _ T  =  showChar 'T'

  showList  =  foldr (.) id . map shows

base :: Char -> Maybe Base
base 'A'  =  Just A
base 'C'  =  Just C
base 'G'  =  Just G
base 'T'  =  Just T
base _    =  Nothing

type DNA      =  [Base]
type Segment  =  [Base]

dna  ::  DNA
dna  =  [A, T, G, T, A, A, A, G, G, G, T, C, C, A, A, T, G, A]

mm  ::  DNA
mm  =  filter base
   "ATGTAAAGGGTCCAATGACTGGAATTACTTCACAGCCCTGACACTGTGGAGAGATGGATA\
   \CCAGTTTAATTATGGACAGCTGAGAATAATCTGGGAGTCACCTGCTCTGGATATTTATCA\
   \TCATTATGAACTTCTCACCAGGCTGTGGCCCGAACACTTTCATAACGTCCCATTTGTGTT\
   \GGGCAGACATTATGATCTATACAGAACCTGCCTTGTCTGTCACCTTTGAATTTATGGATA\
   \GACAATTTAATAT\
   \GTGTTCCTGGCAGCAAAGATAATCATGGAGAGTGGAGAGAAACTAACCTTACCATTGATA\
   \GGGAAACTCTTGAAGTGTCAACTTCTCCATATTAAATCCAAGGACCAGCAGAGACGAGAA\
   \AATGAAAAGAAGATGGTAGAAGAAAGAACAAAATCAGAAAAAGACAAAGGAAAAGGGAAG\
   \TCTCCAAAGGAGAAGAAAGTTGCCAGTGCCAAGCCTGGGAAGGGAAAGAGCAAGGACCAG"

testdna :: DNA
testdna = filter base "ATGTAAAGGGTCCAATGA"

readDNA :: FilePath -> IO [Base]
readDNA path
  =  do  x<- readFile path
         return (filter base x)

contains :: Segment -> DNA -> Bool
contains seg dna = findSubString seg dna 0

findSubString :: Segment -> DNA -> Int -> Bool
findSubString seg dna index
    | index >= length dna = False
    | otherwise = (seg == substring index (index + length seg) dna) || findSubString seg dna (index+1)

substring :: Int -> Int -> [Base] -> [Base]
substring start end dna = take (end - start) (drop start dna)

longestOnlyAs       :: DNA -> Integer
longestOnlyAs dna = toInteger (maximum [length x | x <- (sort $ group dna) , head x == A])

--longestAtMostTenAs  :: DNA -> Integer
--longestAtMostTenAs = 

-- If you want to test your code on a larger example, say within GHCi

-- dna <- readDNA "mm1.dna"
-- longestOnlyAs dna
