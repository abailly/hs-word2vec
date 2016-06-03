{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}
module Words where

import           Control.Arrow       ((***))
import           Control.DeepSeq     (force)
import           Control.Exception   (evaluate, finally)
import           Control.Monad       (foldM)
import           Data.HashMap.Strict (empty)
import           Log
import           Prelude             hiding (readFile)
import           System.IO           (IOMode (..), hClose, hGetContents,
                                      hSetEncoding, openFile, utf8)
import           Words.Dictionary

indexFile :: FilePath -> Index -> IO (Index, [String])
indexFile file dict = do
  h <- openFile file ReadMode
  s <- (do
          progress (TokenizingFile file)
          hSetEncoding h utf8
          s <- hGetContents h
          evaluate $ force s
      ) `finally` hClose h

  let tokens = tokenizeString s
      dict' = s `seq` indexString dict tokens
  progress (TokenizedFile file tokens)
  return $ (dict' `seq` dict', tokens)


-- |Encode the words of several files into a dictionary
tokenizeFiles :: [String]        -- file paths
              -> IO (Dictionary, [[String]])
tokenizeFiles files = do
  progress $ TokenizingFiles (length files)
  !(dict, rtokens) <- (encodeWords *** reverse) <$> foldM tokenizeAndIndex (empty, []) files
  progress $ EncodedDictionary dict
  progress $ TokenizedFiles rtokens
  return (dict, rtokens)
    where
      tokenizeAndIndex (dict, toks) f = do
        (dict', tokens) <- indexFile f dict
        return (dict', tokens:toks)

