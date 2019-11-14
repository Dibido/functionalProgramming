module Main
where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let fileSizes = map printFileSize args -- [IO ()]
  sequence_ fileSizes -- [IO ()] => IO ()
  printFileTotals args
 
printFileSize :: FilePath -> IO ()
printFileSize fileName =
  do
    file <- readFile fileName
    putStr ((show $ length $ lines file) ++ "\t") -- lines
    putStr ((show $ length $ words file) ++ "\t") -- words
    putStr ((show $ length file) ++ "\t") -- bytes
    putStrLn fileName

printFileTotals :: [FilePath] -> IO()
printFileTotals fileNames =
  do
    files <- sequence $ map readFile fileNames -- [String]
    let lineCount = sum [length $ lines file | file <- files]
    let wordCount = sum [length $ words file | file <- files]
    let byteCount = sum [length file | file <- files]
    putStrLn (show lineCount ++ "\t" ++ show wordCount ++ "\t" ++ show byteCount ++"\ttotal")
