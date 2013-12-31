module Words where

import Prelude hiding (readFile)
import System.IO.UTF8(readFile)
import NLP.Tokenize(tokenize)
import Text.Regex.TDFA((=~))
import Data.Char(toLower)
import Data.List(sortBy)
import Data.HashMap.Strict(empty,
                         insertWith,
                         HashMap,
                         toList)


-- |Index a list of words into a frequency map
indexWord :: HashMap String Int -> String -> HashMap String Int
indexWord m w = insertWith (+) w 1 m

-- |Tokenize and normalize a string
tokenizeString :: String -> [ String ]
tokenizeString = map (map toLower).filter (=~ "^[a-zA-Z-]+$").tokenize

-- |Extract the words of a file
tokenizeFile :: String   -- file path
         -> IO [(String,Int)]
tokenizeFile file =
  readFile file >>=
  return.
  sortBy freq.toList.foldl indexWord empty.tokenizeString
  where
    freq a b = compare (snd a) (snd b)
