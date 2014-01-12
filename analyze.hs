module Main where

import Prelude hiding (readFile)
import System.IO(hSetBuffering,BufferMode(..),stdout)
import System.IO.UTF8(readFile)
import System.Directory(getDirectoryContents)
import Data.List(isSuffixOf)
import Control.Monad(foldM)


import Crawl
import Model
import Words

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
  hSetBuffering stdout NoBuffering
  -- pdfs <- downloadPDFs
  -- txts <- (mapM convertToText pdfs >>= return.filter (/= []))
   -- m <- trainFiles txts
  m <- analyzeDirectory "."
  writeFile "model.txt" (show m)
