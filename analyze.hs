module Main where

import Crawl

main :: IO ()
main = runInThreadPool 3 ["1107.0194","1107.0194","1301.0159","1301.0159","1104.3787","1104.3787","1106.6186","1106.6186","1103.1977","1103.1977"]
