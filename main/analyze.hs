module Main where

import           Control.Monad      (foldM, when)
import           Data.List          (isSuffixOf)
import           Prelude            hiding (readFile)
import           System.Directory   (getDirectoryContents)
import           System.Environment (getArgs)
import           System.FilePath    ((</>))
import           System.IO          (BufferMode (..), hSetBuffering, stdout)
import           System.IO.UTF8     (readFile)


import           Crawl
import           Display
import           Model
import           Words

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
        (x:xs) -> x
        []     -> "."
  putStrLn $ "analyzing directory "++ dir
  hSetBuffering stdout NoBuffering
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


