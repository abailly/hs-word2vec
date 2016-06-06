{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
             | WritingPCAFile FilePath
             | WritingDiagram FilePath [ String ]
               -- Training
             | StartTraining Model
             | TrainingSentence Int Int
             | TrainWord String String
             | TrainingWindow Double String [String]
             | InitialWordVector Int Vector
             | BeforeUpdate Int Vector
             | DotProduct Double
             | ErrorGradient Double
             | InputLayerAfterGradient Vector
             | HiddenLayerAfterGradient Vector
             | UpdatedWordVector Int Vector
             | TrainedSentence NominalDiffTime
             | Done
             deriving (Show, Generic)

instance ToJSON NominalDiffTime where
  toJSON dt = toJSON $ (realToFrac dt :: Double)

instance ToJSON Message

class (MonadIO io) => Progress io where
  progress :: Message -> io ()
