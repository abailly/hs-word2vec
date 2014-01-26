import Prelude hiding (readFile)
import System.IO(hSetBuffering,BufferMode(..),stdout)
import System.IO.UTF8(readFile)
import System.Directory(getDirectoryContents)
import System.FilePath((</>))
import System.Environment(getArgs)
import Data.List(isSuffixOf)
import Control.Monad(when)
import System.Console.GetOpt

import Graphics.Rendering.Chart.Backend.Diagrams


import Model
import Words
import Display

pca :: String -> IO  [(String, Double, Double)]
pca file = analyze file >>= return . pcaAnalysis
  
analyze :: String -> IO Model
analyze file = do
  content <- readFile file  >>= return.tokenizeString 
  dict <- tokenizeFiles [file]
  trainModel 0 dict [content]

trainFiles :: [String] -> IO Model
trainFiles txts = do
  dict <- tokenizeFiles txts
  putStrLn $ "Encoded " ++ (show $ dictionaryLength dict) ++ " words, dim="++  (show $ encodingLength dict) 
  contents <- mapM (\ f -> readFile f >>= return. tokenizeString) txts
  let tokens = length contents
      
  putStrLn $ "Training " ++ (show tokens) ++ " files"
  trainModel tokens dict contents

analyzeDirectory :: String -> IO Model
analyzeDirectory dir = do
  txts <- getDirectoryContents dir >>= return.filter (isSuffixOf ".txt")
  trainFiles txts

data Config = CorpusDir String
            | Verbose
            | Version


options :: [OptDescr Config]
options =
     [ Option ['v']     ["verbose"]    (NoArg Verbose)          "chatty output on stderr"
     , Option ['V','?'] ["version"]    (NoArg Version)          "show version number"
     , Option ['d']     ["corpus-dir"] (ReqArg CorpusDir "DIR") "corpus directory FILE"

     ]

word2vecOpts :: [String] -> IO ([Config], [String])
word2vecOpts argv = 
       case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: word2vec [OPTION...] [words...]"


corpusDir :: [Config] -> String
corpusDir []              = "."
corpusDir (CorpusDir d:_) = d
corpusDir (_:configs)     = corpusDir configs

main :: IO ()
main = do
  args <- getArgs
  (configs,selectedWords) <- word2vecOpts args
  let dir = corpusDir configs
  putStrLn $ "analyzing directory "++ dir 
  hSetBuffering stdout NoBuffering

  m <- analyzeDirectory dir
  let p = pcaAnalysis m
  let chart = drawSelectedWords p selectedWords
  when (length p /= (numberOfWords m)) 
    (fail $ "PCA should have same number of words than model: "++ (show $ length p) ++ "vs. " ++ (show $ numberOfWords m))

  let modelFile = (dir </> "model.vec")
  let pcaFile = (dir </> "model.pca")
  let diagramFile = (dir </> "model.svg")

  putStrLn $ "Writing model to file "++ modelFile
  writeFile modelFile (show m)
  
  putStrLn $ "Writing PCA to file " ++ pcaFile
  writeFile pcaFile (show p)

  putStrLn $ "Writing vector space diagram " ++ diagramFile ++ "for words " ++ (show selectedWords)
  renderableToSVGFile chart 1000 1000 diagramFile

  putStrLn "done"

  
