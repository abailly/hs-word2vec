module Main where

import Prelude hiding (readFile)
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
  trainModel dict [content]

trainFiles :: [String] -> IO Model
trainFiles txts = do
  dict <- tokenizeFiles txts
  contents <- mapM (\ f -> readFile f >>= return. tokenizeString) txts
  trainModel dict contents

analyzeDirectory :: String -> IO Model
analyzeDirectory dir = do
  txts <- getDirectoryContents dir >>= return.filter (isSuffixOf ".txt")
  trainFiles txts
  
main :: IO ()
main = do
  pdfs <- downloadPDFs
  txts <- (mapM convertToText pdfs >>= return.filter (/= []))
  m <- trainFiles txts
  -- need to save model...
  putStrLn "done"
