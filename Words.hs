module Words where

import Prelude hiding (readFile)
import System.IO.UTF8(readFile)
import NLP.Tokenize(tokenize)
import Text.Regex.TDFA((=~))

-- |Extract the words of a file
tokenizeFile :: String   -- file path
         -> IO [ String ]
tokenizeFile file = readFile file >>= return.filter (=~ "^[a-zA-Z]+$").tokenize
