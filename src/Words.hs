{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}
module Words where

import           Control.Exception   (finally)
import           Control.Monad       (foldM)
import           Data.HashMap.Strict (empty, size, size)
import           Prelude             hiding (readFile)
import           System.IO           (IOMode (..), hClose, hGetContents,
                                      hSetEncoding, openFile, utf8)
import           Words.Dictionary

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

