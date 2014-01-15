import Crawl

main :: IO ()
main = do 
  pdfs <- downloadPDFs
  txts <- (mapM convertToText pdfs >>= return.filter (/= []))
  putStrLn  $ "Done crawling " ++ (show $ length txts)
