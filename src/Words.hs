{-# LANGUAGE BangPatterns #-}
module Words where

import           Control.Arrow       ((***))
import           Control.DeepSeq     (force)
import           Control.Exception   (evaluate, finally)
import           Control.Monad       (foldM)
import           Control.Monad.Trans (liftIO)
import           Data.HashMap.Strict (empty)
import           Log
import           Prelude             hiding (readFile)
import           System.IO           (IOMode (..), hClose, hGetContents,
                                      hSetEncoding, openFile, utf8)
import           Words.Dictionary

indexFile :: (Progress m) => FilePath -> Index -> m (Index, [String])
indexFile file dict = do
  h <- liftIO $ openFile file ReadMode
  progress Middle (TokenizingFile file)
  s <- liftIO $ (do hSetEncoding h utf8
                    s <- hGetContents h
                    evaluate $ force s
                ) `finally` hClose h

  let tokens = tokenizeString s
      dict' = s `seq` indexString dict tokens
  progress Middle (TokenizedFile file tokens)
  return  (dict', tokens)


-- |Encode the words of several files into a dictionary
tokenizeFiles :: (Progress m)
                 => [String]        -- file paths
                 -> m (Dictionary, [[String]])
tokenizeFiles files = do
  progress Middle $ TokenizingFiles (length files)
  !(dict, rtokens) <- (encodeWords *** reverse) <$> foldM tokenizeAndIndex (empty, []) files
  progress Middle $ EncodedDictionary dict
  progress Middle $ TokenizedFiles rtokens
  return (dict, rtokens)
    where
      tokenizeAndIndex (dict, toks) f = do
        (dict', tokens) <- indexFile f dict
        return (dict', tokens:toks)

