{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

import           Control.Monad                             (when)
import           Data.Aeson
import qualified Data.ByteString.Lazy                      as BS
import qualified Data.ByteString.Lazy.Char8                as BS8
import           Display
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Log
import           Model
import           Model.IO
import           Model.Types
import           Options.Generic
import           Prelude                                   hiding (readFile)
import           System.Directory                          (doesFileExist)
import           System.FilePath                           ((</>))
import           System.IO                                 (BufferMode (..),
                                                            hSetBuffering,
                                                            readFile, stdout)

instance Progress IO where
  progress =  BS8.putStrLn . encode

data Config = Config { corpusDirectory  :: FilePath
                     , verbosity        :: Bool
                     , stepByStep       :: Bool
                     , numberOfFeatures :: Int
                     , selectedWords    :: [ String ]
                     } deriving (Generic)

instance ParseRecord Config

defaultConfig :: [String] -> Config
defaultConfig = Config "." False False 100

runAnalysis :: Config -> IO Model
runAnalysis c@(stepByStep -> False) = analyzeDirectory (numberOfFeatures c) (corpusDirectory c)
runAnalysis c@(stepByStep -> True)  = analyzeDirectory (numberOfFeatures c) (corpusDirectory c)

main :: IO ()
main = do
  config <- getRecord "Word2Vec Trainer"
  let dir = corpusDirectory config
      modelFile = (dir </> "model.vec")
      pcaFile = (dir </> "model.pca")
      diagramFile = (dir </> "model.svg")

  progress $ AnalyzingDirectory dir
  hSetBuffering stdout NoBuffering

  hasModel <- doesFileExist modelFile

  m <- if hasModel then
          read `fmap` readFile modelFile
       else
         runAnalysis config

  let p = pcaAnalysis m
      top100 = mostFrequentWords 100 m
      chart = drawSelectedWords p (if null (selectedWords config) then top100 else (selectedWords config))
  when (length p /= (numberOfWords m))
    (fail $ "PCA should have same number of words than model: "++ (show $ length p) ++ "vs. " ++ (show $ numberOfWords m))

  progress $ WritingModelFile modelFile
  writeFile modelFile (show m)

  progress $ WritingPCAFile pcaFile
  writeFile pcaFile (show p)

  progress $ WritingDiagram diagramFile (selectedWords config)

  (bs, _) <- renderableToSVGString chart 1000 1000
  BS.writeFile diagramFile bs

  progress Done


