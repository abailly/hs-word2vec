{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeSynonymInstances      #-}
-- | Provides structured logging and report on training process.
--
-- This module mainly exposes a type for `Message`s that are output by `word2vec` when
-- working on input and training the underlying model and an interface mechanism for
-- publishing those messages using any underlying `MonadIO` instance.
module Log where

import           Control.Monad.Trans (MonadIO)
import           Data.Aeson
import           Data.Time.Clock
import           GHC.Generics
import           Model.Types
import           Words.Dictionary

-- |All type of messages emitted by application while working.
data Message = AnalyzingDirectory FilePath
             | EncodedDictionary Dictionary
             | TokenizingFiles Int
             | TokenizingFile FilePath
             | TokenizedFile FilePath [ String ]
             | TokenizedFiles [[String]]
             | WritingModelFile FilePath
             | LoadingModelFile FilePath
             | WritingPCAFile FilePath
             | WritingDiagram FilePath [ String ]
               -- Training
             | StartTraining Int
             | TrainingSentence Int Int
             | TrainWord String String
             | TrainingWindow Double String [String]
             | InitialWordVector Int WordVector
             | BeforeUpdate Int WordVector
             | DotProduct Double
             | ErrorGradient Double
             | InputLayerAfterGradient WordVector
             | HiddenLayerAfterGradient WordVector
             | UpdatedWordVector Int WordVector
             | TrainedSentence NominalDiffTime
             | Done
             deriving (Show, Generic)

instance ToJSON Message

data Level = Coarse
           | Middle
           | Fine
           deriving (Eq, Show, Read, Enum)

class (MonadIO io) => Progress io where
  progress :: Level -> Message -> io ()
