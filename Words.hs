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
import Huffman

-- |Index a list of words into a frequency map
indexWord :: HashMap String Int -> String -> HashMap String Int
indexWord m w = insertWith (+) w 1 m

-- |Tokenize and normalize a string
tokenizeString :: String -> [ String ]
tokenizeString = map (map toLower).filter (=~ "^[a-zA-Z-]+$").tokenize

-- |Encode the words of a file
tokenizeFile :: String   -- file path
         -> IO (HashMap String Coding)
tokenizeFile file =
  readFile file >>=
  return.
  huffmanEncode . foldl indexWord empty.tokenizeString
