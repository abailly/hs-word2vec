import           Control.Monad                             (when)
import qualified Data.ByteString.Lazy                      as BS
import           Data.List                                 (isSuffixOf)
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Prelude                                   hiding (readFile)
import           System.Console.GetOpt
import           System.Directory                          (doesFileExist, getDirectoryContents)
import           System.Environment                        (getArgs)
import           System.FilePath                           ((</>))
import           System.IO                                 (BufferMode (..),
                                                            IOMode (..), hClose,
                                                            hGetContents,
                                                            hSetBuffering,
                                                            hSetEncoding,
                                                            openFile, readFile,
                                                            stdout, utf8)

import           Display
import           Model
import           Words

trainFiles :: [String] -> IO Model
trainFiles txts = do
  dict <- tokenizeFiles txts
  putStrLn $ "Encoded " ++ (show $ dictionaryLength dict) ++ " words, dim="++  (show $ encodingLength dict)
  contents <- mapM tokenizeFile txts
  let tokens = length contents
  putStrLn $ "Training " ++ (show tokens) ++ " files"
  trainModel tokens dict contents
    where
      tokenizeFile f = do
        h <- openFile f ReadMode
        putStrLn ("Tokenizing " ++ f)
        hSetEncoding h utf8
        s <- hGetContents h
        putStrLn $ "read " ++ show (length s) ++ " chars from " ++ f
        hClose h
        let dict' = s `seq` (tokenizeString s)
        putStrLn $ "Tokenized " ++ f ++  "to " ++ show (length dict')  ++ " words"
        return $ dict' `seq` dict'

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
  putStrLn $ "analyzing directory "++ dir
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


  putStrLn $ "Writing model to file "++ modelFile
  writeFile modelFile (show m)

  putStrLn $ "Writing PCA to file " ++ pcaFile
  writeFile pcaFile (show p)

  putStrLn $ "Writing vector space diagram " ++ diagramFile ++ "for words " ++ (show selectedWords)
  (bs, _) <- renderableToSVGString chart 1000 1000
  BS.writeFile diagramFile bs

  putStrLn "done"


