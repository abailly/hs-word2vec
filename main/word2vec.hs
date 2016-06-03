{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad                             (when)
import qualified Data.ByteString.Lazy                      as BS
import           Data.List                                 (isSuffixOf)
import           Display
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Log
import           Model
import           Model.Types
import           Options.Generic
import           Prelude                                   hiding (readFile)
import           System.Directory                          (doesFileExist, getDirectoryContents)
import           System.FilePath                           ((</>))
import           System.IO                                 (BufferMode (..),
                                                            hSetBuffering,
                                                            readFile, stdout)
import           Words

trainFiles :: Int -> [String] -> IO Model
trainFiles numFeatures txts = do
  (dict, contents) <- tokenizeFiles txts
  progress (EncodedDictionary dict)
  trainModel numFeatures dict contents

analyzeDirectory :: Int -> String -> IO Model
analyzeDirectory numFeatures dir = do
  txts <- getDirectoryContents dir >>= return.filter (isSuffixOf ".txt")
  trainFiles numFeatures $ map (dir </>) txts

data Config = Config { corpusDirectory  :: FilePath
                     , verbosity        :: Bool
                     , numberOfFeatures :: Int
                     , selectedWords    :: [ String ]
                     } deriving (Generic)

instance ParseRecord Config

defaultConfig :: [String] -> Config
defaultConfig = Config "." False 100

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
         analyzeDirectory (numberOfFeatures config) dir

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

  progress $ Done


