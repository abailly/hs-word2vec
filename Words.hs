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
                         toList,
                         size,
                         elems)
import Control.Monad(foldM)
import Huffman

data Dictionary = Dict {
  dictionary :: HashMap String Coding,
  dictionaryLength :: Int,
  encodingLength :: Int } deriving (Eq, Show)
  
emptyDictionary :: HashMap String Coding
emptyDictionary = empty

-- |Index a list of words into a frequency map
indexWord :: HashMap String Int -> String -> HashMap String Int
indexWord m w = insertWith (+) w 1 m

-- |Tokenize and normalize a string
tokenizeString :: String -> [ String ]
tokenizeString = map (map toLower).filter (=~ "^[a-zA-Z-]+$").tokenize

indexString :: HashMap String Int -> String -> HashMap String Int
indexString dict = foldl indexWord dict . tokenizeString

-- |Encode the words of several files into a dictionary
tokenizeFiles :: [String]        -- file paths
             -> IO Dictionary
tokenizeFiles files = do
  dictionary <- foldM (\ dict f -> readFile f >>= return.indexString dict) empty files
  let encoding = huffmanEncode $ dictionary
  return $ Dict encoding (size encoding) (length $ huffman $ head $ elems encoding)
      
