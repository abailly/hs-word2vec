import           Control.Monad                             (when)
import qualified Data.ByteString.Lazy                      as BS
import           Data.List                                 (isSuffixOf)
import           Display
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Log
import           Model
import           Model.Types
import           Prelude                                   hiding (readFile)
import           System.Console.GetOpt
import           System.Directory                          (doesFileExist, getDirectoryContents)
import           System.Environment                        (getArgs)
import           System.FilePath                           ((</>))
import           System.IO                                 (BufferMode (..),
                                                            hSetBuffering,
                                                            readFile, stdout)
import           Words

trainFiles :: [String] -> IO Model
trainFiles txts = do
  (dict, contents) <- tokenizeFiles txts
  progress (EncodedDictionary dict)
  let tokens = length contents
  trainModel tokens dict contents

analyzeDirectory :: String -> IO Model
analyzeDirectory dir = do
  txts <- getDirectoryContents dir >>= return.filter (isSuffixOf ".txt")
  trainFiles $ map (dir </>) txts

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
      modelFile = (dir </> "model.vec")
      pcaFile = (dir </> "model.pca")
      diagramFile = (dir </> "model.svg")

  progress $ AnalyzingDirectory dir
  hSetBuffering stdout NoBuffering

  hasModel <- doesFileExist modelFile

  m <- if hasModel then
          read `fmap` readFile modelFile
       else
         analyzeDirectory dir

  let p = pcaAnalysis m
      top100 = mostFrequentWords 100 m
      chart = drawSelectedWords p (if null selectedWords then top100 else selectedWords)
  when (length p /= (numberOfWords m))
    (fail $ "PCA should have same number of words than model: "++ (show $ length p) ++ "vs. " ++ (show $ numberOfWords m))

  progress $ WritingModelFile modelFile
  writeFile modelFile (show m)

  progress $ WritingPCAFile pcaFile
  writeFile pcaFile (show p)

  progress $ WritingDiagram diagramFile selectedWords

  (bs, _) <- renderableToSVGString chart 1000 1000
  BS.writeFile diagramFile bs

  progress $ Done


