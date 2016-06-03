{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}
module Words where

import           Control.Exception   (finally)
import           Control.Monad       (foldM)
import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Char           (toLower)
import           Data.HashMap.Strict (HashMap, elems, empty, insertWith, size,
                                      size, toList)
import           Data.List           (intersperse, sortBy)
import           Data.Ord            (comparing)
import           Debug.Trace
import           GHC.Generics
import           Huffman
import           NLP.Tokenize        (tokenize)
import           Prelude             hiding (readFile)
import           System.IO           (IOMode (..), hClose, hGetContents,
                                      hSetEncoding, openFile, utf8, withFile)
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
encodeWords dictionary = let encoding = huffmanEncode $ dictionary
                             encodingLength = maximum (map (length . unCode . huffman) $ elems encoding)
                         in Dict encoding (size encoding) encodingLength

indexFile :: FilePath -> Index -> IO (Index, [String])
indexFile file dict = do
  h <- openFile file ReadMode
  s <- (do
          putStrLn ("Tokenizing " ++ file)
          hSetEncoding h utf8
          s <- hGetContents h
          putStrLn $ "read " ++ show (length s) ++ " chars from " ++ file
          return s
      ) `finally` hClose h

  let tokens = tokenizeString s
      dict' = s `seq` indexString dict tokens
  putStrLn $ "Indexed dictionary with " ++ show (size dict')  ++ " words"
  return $ (dict' `seq` dict', tokens)


-- |Encode the words of several files into a dictionary
tokenizeFiles :: [String]        -- file paths
              -> IO (Dictionary, [[String]])
tokenizeFiles files = do
  putStrLn ("Tokenizing " ++ show (length files) ++ " files")
  !(dictionary, rtokens) <- foldM tokenizeAndIndex (empty, []) files
  putStrLn $ "Encoding dictionary: " ++ (show $ size dictionary)
  return $ (encodeWords dictionary, reverse rtokens)
    where
      tokenizeAndIndex (dict, toks) f = do
        (dict', tokens) <- indexFile f dict
        return (dict', tokens:toks)

