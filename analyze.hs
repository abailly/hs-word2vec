module Main where

import Crawl

main :: IO ()
main = do
  pdfs <- downloadPDFs
  txts <- (mapM convertToText pdfs >>= return.filter (/= []))
  
