{-# LANGUAGE DeriveGeneric #-}
module Words.Dictionary(
  -- * Types
  Dictionary(Dict), emptyDictionary, Index,
  -- * Tokenizing
  tokenizeString,
  -- * Indexing
  indexString, encodeWords,
  -- * Querying
  dictionary, dictionaryLength, encodingLength,
  orderedWords
  ) where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Char           (toLower)
import           Data.HashMap.Strict (HashMap, elems, empty, insertWith, size,
                                      size, toList)
import           Data.List           (sortBy)
import           Data.Ord            (comparing)
import           GHC.Generics
import           Huffman
import           NLP.Tokenize        (tokenize)
import           Prelude             hiding (readFile)
import           Text.Regex.TDFA     ((=~))

type Index = HashMap String Int

data Dictionary = Dict {
  dictionary       :: HashMap String Coding,
  dictionaryLength :: Int,
  encodingLength   :: Int } deriving (Eq, Show, Read, Generic)

instance ToJSON Dictionary
instance FromJSON Dictionary

emptyDictionary :: Dictionary
emptyDictionary = Dict empty 0 0

-- | Return a list of all words in dictionary in ascending order of their index.
--
-- >>> orderedWords (encodeWords $ indexString empty "some words for testing words")
-- ["words","some","for","testing"]
orderedWords :: Dictionary -> [ String ]
orderedWords (Dict d _ _)  = map fst $ sortBy (comparing (index.snd)) (toList d)

-- |Index a list of words into a frequency map
indexWord :: Index -> String -> Index
indexWord m w = insertWith (+) w 1 m

-- |Tokenize and normalize a string
tokenizeString :: String -> [ String ]
tokenizeString = map (map toLower).filter (=~ "^[a-zA-Z'-]+$").tokenize

-- |Update a frequency map with tokens from given string.
indexString :: Index -> [ String ] -> Index
indexString = foldl indexWord

encodeWords :: Index -> Dictionary
encodeWords dict = let encoding = huffmanEncode $ dict
                       maxCodeLength = maximum (map (length . unCode . huffman) $ elems encoding)
                   in Dict encoding (size encoding) maxCodeLength
