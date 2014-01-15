import Prelude hiding (readFile)
import System.IO(hSetBuffering,BufferMode(..),stdout)
import System.IO.UTF8(readFile)
import System.Directory(getDirectoryContents)
import System.FilePath((</>))
import System.Environment(getArgs)
import Data.List(isSuffixOf)
import Control.Monad(when)


import Model
import Words
import Display

pca :: String -> IO  [(String, Double, Double)]
pca file = analyze file >>= pcaAnalysis
  
analyze :: String -> IO Model
analyze file = do
  content <- readFile file  >>= return.tokenizeString 
  dict <- tokenizeFiles [file]
  trainModel 0 dict [content]

trainFiles :: [String] -> IO Model
trainFiles txts = do
  dict <- tokenizeFiles txts
  putStrLn $ "Encoded " ++ (show $ dictionaryLength dict) ++ " words, dim="++  (show $ encodingLength dict) 
  contents <- mapM (\ f -> readFile f >>= return. tokenizeString) txts
  let tokens = length contents
      
  putStrLn $ "Training " ++ (show tokens) ++ " files"
  trainModel tokens dict contents

analyzeDirectory :: String -> IO Model
analyzeDirectory dir = do
  txts <- getDirectoryContents dir >>= return.filter (isSuffixOf ".txt")
  trainFiles txts
  
main :: IO ()
main = do
  args <- getArgs
  let dir = case args of
        (x:_) -> x
        []    -> "."
  putStrLn $ "analyzing directory "++ dir 
  hSetBuffering stdout NoBuffering
  -- pdfs <- downloadPDFs
  -- txts <- (mapM convertToText pdfs >>= return.filter (/= []))
   -- m <- trainFiles txts
  m <- analyzeDirectory dir
  p <- pcaAnalysis m
  when (length p /= (numberOfWords m)) 
    (fail $ "PCA should have same number of words than model: "++ (show $ length p) ++ "vs. " ++ (show $ numberOfWords m))

  let modelFile = (dir </> "model.vec")
  let pcaFile = (dir </> "model.pca")
  putStrLn $ "Writing model to file "++ modelFile
  writeFile modelFile (show m)
  putStrLn $ "Writing PCA to file " ++ pcaFile
  writeFile pcaFile (show p)

  
