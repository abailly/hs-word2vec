module Words where

import Prelude hiding (readFile)
import System.IO.UTF8(readFile)
import NLP.Tokenize(tokenize)
import Text.Regex.TDFA((=~))
import Data.Char(toLower)
import Data.List(sortBy, intersperse)
import Data.Ord(comparing)
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
  encodingLength :: Int } deriving (Eq)

instance Show Dictionary where
  show (Dict dict size len) = concat $ intersperse "," [show $ toList dict,  show size, show len]
    
emptyDictionary :: Dictionary
emptyDictionary = Dict empty 0 0 

-- | Return a list of all words in dictionary in ascending order of their index.
--
-- >>> orderedWords (encodeWords $ indexString empty "some words for testing words") 
-- ["words","some","for","testing"]
orderedWords :: Dictionary -> [ String ]
orderedWords (Dict d _ _)  = map fst $ sortBy (comparing (index.snd)) (toList d)
  
-- |Index a list of words into a frequency map
indexWord :: HashMap String Int -> String -> HashMap String Int
indexWord m w = insertWith (+) w 1 m

-- |Tokenize and normalize a string
tokenizeString :: String -> [ String ]
tokenizeString = map (map toLower).filter (=~ "^[a-zA-Z-]+$").tokenize

-- |Update a frequency map with tokens from given string.
indexString :: HashMap String Int -> String -> HashMap String Int
indexString dict = foldl indexWord dict . tokenizeString

encodeWords :: HashMap String Int -> Dictionary
encodeWords dictionary = let encoding = huffmanEncode $ dictionary
                             encodingLength = maximum (map (length . huffman) $ elems encoding)
                         in Dict encoding (size encoding) encodingLength

-- |Encode the words of several files into a dictionary
tokenizeFiles :: [String]        -- file paths
             -> IO Dictionary
tokenizeFiles files = do
  dictionary <- foldM (\ dict f -> putStrLn ("Tokenizing " ++ f) >> readFile f >>= return.indexString dict) empty files
  putStrLn $ "Encoding dictionary: " ++ (show $ size dictionary)
  return $ encodeWords dictionary
      
