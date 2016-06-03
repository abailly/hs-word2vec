{-# LANGUAGE DeriveGeneric #-}
-- | Provides structured logging and report on training process.
--
-- This module mainly exposes a type for `Message`s that are output by `word2vec` when
-- working on input and training the underlying model and an interface mechanism for
-- publishing those messages using any underlying `MonadIO` instance.
module Log where

import           Control.Monad.Trans (MonadIO)
import           Data.Aeson
import           GHC.Generics
import           Words

-- |All type of messages emitted by application while working.
data Message = EncodedDictionary Dictionary
             | TokenizingFile FilePath
             | TokenizedFile FilePath [ String ]
             deriving (Show, Generic)

instance ToJSON Message
instance FromJSON Message

class (MonadIO io) => Progress io where
  progress :: Message -> io ()

