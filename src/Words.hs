{-# LANGUAGE BangPatterns #-}
module Words where

import           Control.Exception   (finally)
import           Control.Monad       (foldM)
import           Data.Char           (toLower)
import           Data.HashMap.Strict (HashMap, elems, empty, insertWith, size,
                                      size, toList)
import           Data.List           (intersperse, sortBy)
import           Data.Ord            (comparing)
import           Debug.Trace
import           Huffman
import           NLP.Tokenize        (tokenize)
import           Prelude             hiding (readFile)
import           System.IO           (IOMode (..), hClose, hGetContents,
                                      hSetEncoding, openFile, utf8, withFile)
import           Text.Regex.TDFA     ((=~))

data Dictionary = Dict {
  dictionary       :: HashMap String Coding,
  dictionaryLength :: Int,
  encodingLength   :: Int } deriving (Eq, Show, Read)

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
tokenizeString = map (map toLower).filter (=~ "^[a-zA-Z'-]+$").tokenize

-- |Update a frequency map with tokens from given string.
indexString :: HashMap String Int -> String -> HashMap String Int
indexString dict s = foldl indexWord dict $  tokenizeString s

encodeWords :: HashMap String Int -> Dictionary
encodeWords dictionary = let encoding = huffmanEncode $ dictionary
                             encodingLength = maximum (map (length . unCode . huffman) $ elems encoding)
                         in Dict encoding (size encoding) encodingLength

-- |Encode the words of several files into a dictionary
tokenizeFiles :: [String]        -- file paths
             -> IO Dictionary
tokenizeFiles files = do
  putStrLn ("Tokenizing " ++ show (length files) ++ " files")
  !dictionary <- foldM indexFile empty files
  putStrLn $ "Encoding dictionary: " ++ (show $ size dictionary)
  return $ encodeWords dictionary
    where
      indexFile dict f = do
        h <- openFile f ReadMode
        s <- (do
            putStrLn ("Tokenizing " ++ f)
            hSetEncoding h utf8
            s <- hGetContents h
            putStrLn $ "read " ++ show (length s) ++ " chars from " ++ f
            return s
          )
          `finally` hClose h

        let dict' = s `seq` (indexString dict s)
        putStrLn $ "Indexed dictionary with " ++ show (size dict')  ++ " words"
        return $ dict' `seq` dict'

