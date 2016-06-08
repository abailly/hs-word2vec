{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

import           Control.Monad                             (when)
import           Control.Monad.Reader
import           Control.Monad.Trans                       (MonadIO, lift,
                                                            liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy                      as BS
import qualified Data.ByteString.Lazy.Char8                as BS8
import           Data.Functor                              (void)
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

instance Progress (ReaderT Int IO) where
  progress lvl m =  do
    logLevel <- ask
    when (fromEnum lvl <= logLevel) $ liftIO (BS8.putStrLn $ encode m)

newtype Step m a = Step { runStep :: m a }
                 deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans Step  where
  lift = Step

mapStep :: (m a -> m b) -> Step m a -> Step m b
mapStep f (Step m) = Step $ f m

instance (MonadReader a m) => MonadReader a (Step m) where
  reader = lift . reader
  local = mapStep . local

instance (MonadIO m, MonadReader Int m) => Progress (Step m) where
  progress lvl m = Step $ do
    logLevel <- ask
    when (fromEnum lvl <= logLevel) $ liftIO $ do
      BS8.putStrLn $ encode m
      void getLine

data Config = Config { corpusDirectory  :: FilePath
                     , verbosity        :: Int
                     , stepByStep       :: Bool
                     , numberOfFeatures :: Int
                     , selectedWords    :: [ String ]
                     } deriving (Generic)

instance ParseRecord Config

defaultConfig :: [String] -> Config
defaultConfig = Config "." 0 False 100

main :: IO ()
main = do
  config <- getRecord "Word2Vec Trainer"
  go config

go :: Config -> IO ()
go c@(stepByStep -> False) = runReaderT (runAnalysis c)           (verbosity c)
go c@(stepByStep -> True)  = runReaderT (runStep $ runAnalysis c) (verbosity c)

runAnalysis :: (MonadIO m, MonadReader Int m, Progress m) => Config -> m ()
runAnalysis config = do
  let dir         = corpusDirectory config
      modelFile   = dir </> "model.vec"
      pcaFile     = dir </> "model.pca"
      diagramFile = dir </> "model.svg"

  liftIO $ hSetBuffering stdout NoBuffering

  hasModel <- liftIO $ doesFileExist modelFile

  m <- if hasModel
       then do
    progress Coarse $ LoadingModelFile modelFile
    read `fmap` liftIO (readFile modelFile)
       else do
    progress Coarse $ AnalyzingDirectory dir
    analyzeDirectory (numberOfFeatures config) (corpusDirectory config)

  let p = pcaAnalysis m
      top100 = mostFrequentWords 100 m
      chart = drawSelectedWords p (if null ( selectedWords config) then top100 else selectedWords config)
  when (length p /= numberOfWords m)
    (fail $ "PCA should have same number of words than model: "++ show (length p) ++ " vs. " ++ show (numberOfWords m))

  progress Coarse $ WritingModelFile modelFile
  liftIO $ writeFile modelFile (show m)

  progress Coarse $ WritingPCAFile pcaFile
  liftIO $ writeFile pcaFile (show p)

  progress Coarse $ WritingDiagram diagramFile (selectedWords config)

  (bs, _) <- liftIO $ renderableToSVGString chart 1000 1000
  liftIO $ BS.writeFile diagramFile bs

  progress Coarse Done


