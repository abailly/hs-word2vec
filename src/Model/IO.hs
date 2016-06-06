module Model.IO where

import           Control.Monad.Trans (liftIO)
import           Data.List
import           Log
import           Model
import           Model.Types
import           System.Directory
import           System.FilePath
import           Words

trainFiles :: (Progress m) => Int -> [String] -> m Model
trainFiles numFeatures txts = do
  (dict, contents) <- tokenizeFiles txts
  progress Fine (EncodedDictionary dict)
  trainModel numFeatures dict contents

analyzeDirectory :: (Progress m) => Int -> String -> m Model
analyzeDirectory numFeatures dir = do
  txts <- filter (isSuffixOf ".txt") <$> liftIO (getDirectoryContents dir)
  trainFiles numFeatures $ map (dir </>) txts
