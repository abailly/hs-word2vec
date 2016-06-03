{-# LANGUAGE DeriveGeneric #-}
-- | Provides structured logging and report on training process.
--
-- This module mainly exposes a type for `Message`s that are output by `word2vec` when
-- working on input and training the underlying model and an interface mechanism for
-- publishing those messages using any underlying `MonadIO` instance.
module Log where

import           Control.Monad.Trans        (MonadIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS8
import           GHC.Generics
import           Words.Dictionary

-- |All type of messages emitted by application while working.
data Message = AnalyzingDirectory FilePath
             | EncodedDictionary Dictionary
             | TokenizingFiles Int
             | TokenizingFile FilePath
             | TokenizedFile FilePath [ String ]
             | TokenizedFiles [[String]]
             | WritingModelFile FilePath
             | WritingPCAFile FilePath
             | WritingDiagram FilePath [ String ]
             | Done
             deriving (Show, Generic)

instance ToJSON Message
instance FromJSON Message

class (MonadIO io) => Progress io where
  progress :: Message -> io ()


instance Progress IO where
  progress =  BS8.putStrLn . encode