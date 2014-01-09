module Main where

import Prelude hiding (readFile)
import System.IO.UTF8(readFile)

import Control.Monad(foldM)

import Crawl
import Model
import Words

analyze :: String -> IO Model
analyze file = do
  content <- readFile file  >>= return.tokenizeString 
  dict <- tokenizeFiles [file]
  trainModel dict [content]

    
main :: IO ()
main = do
  pdfs <- downloadPDFs
  txts <- (mapM convertToText pdfs >>= return.filter (/= []))
  dict <- tokenizeFiles txts
  contents <- mapM (\ f -> readFile f >>= return. tokenizeString) txts
  m <- trainModel dict contents
  putStrLn "done"
