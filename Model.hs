{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
-- |Neural network based model of words similarity
module Model where
import Control.Monad(foldM)
import Control.Concurrent(threadDelay)
import System.Random(random,randomR,getStdGen,RandomGen,mkStdGen)
import Data.Time.Clock(getCurrentTime,
                       diffUTCTime)
import qualified Data.HashMap.Strict as M
import Matrix
import Huffman
import Window
import Words

data Model = Model {
  -- Number of words in the model
  numberOfWords :: Int,
  
  -- Size of the model or number of dimensions each word is mapped to
  modelSize :: Int,
  
  -- The input -> hidden connection matrix
  -- input layer has size equal to number of words in vocabulary, with each
  -- cell connected to a number of hidden cells equal to the 'dimension' of the model
  -- eg, the number of features we want to track defaulting to 100
  --
  -- syn0 is the original name in C word2vec implementation
  syn0 :: !Matrix,

  -- The hidden -> output connection matrix
  --
  -- syn1 is the original name in C word2vec implementation
  syn1 :: !Matrix,

  -- The dictionary
  -- Each word is mapped to a Coding structure containing, among other things,
  -- the Huffman encoding of the word and references to inner nodes this word is connected to
  -- contains also number of words and maximal length of coding vectors
  vocabulary :: !Dictionary,

  -- Size of training window
  window :: Int
  } deriving (Show)

defaultWindow :: Int
defaultWindow = 10

-- | Raw coefficients of given word
--
coefficient :: (Monad m) => Model -> String -> m Matrix
coefficient m w = do
  let h = dictionary $ vocabulary m
  let Just (Coding index _ huff points) = M.lookup w h
  subMatrix [index] (syn0 m)
  
-- | Compute similarity between two words
--
-- Uses the cosine similarity, eg. dot product between the two vectors. The vectors should be of
-- norm.
similarity :: (Monad m) => Model -> String -> String -> m Double
similarity m u v = do
  u'<- coefficient m u
  v'<- coefficient m v
  let vecU = unitVector u'
  let vecV = unitVector v'
  return $ vecU `dotProduct` vecV

-- | Train a model using a dictionary and a list of sentences
trainModel :: Int -> Dictionary -> [[String]] -> IO Model
trainModel numTokens dict sentences = do
  theModel <- fromDictionary dict
  let alpha = 0.001
  putStrLn $ "Start training model " ++ (show (numberOfWords theModel, modelSize theModel))
  foldM (trainSentence alpha) (0, theModel) sentences >>= return.snd
  

-- |Train given model with a single sentence.
--
-- This function updates the model using skip-gram hierarchical softmax model on a single sentence
-- For each word in the sentence:
--
-- * Define a random training window around the word whose default value is 10
-- * Iterate over all words in the window, updating the underlying neural network
--
-- The updated model is returned along with the total number of words it has been trained on.
trainSentence :: Double
              -> (Int, Model)
              -> [String]
              -> IO (Int, Model)
trainSentence alpha (count,!m) sentence = do
  let len = length sentence
  start <- getCurrentTime
  putStr $ "Training " ++ (show len) ++ "/" ++ (show count) ++ " words"
  g <- getStdGen
  m'<- foldM (trainWindow alpha) m (slidingWindows (window m) g sentence)
  end <- getCurrentTime
  putStrLn $ " in " ++ (show $ diffUTCTime end start)
  return (count + len,m')

      
trainWindow :: Double              -- alpha threshold 
          -> Model               -- model to train
          -> (String,[String])   -- prefix, word, suffix to select window around word
          -> IO Model            -- updated model
trainWindow alpha !m (word,words) = 
  foldM (trainWord alpha word) m (filter (/= word) words)

trainWord :: Double    -- alpha threshold 
          -> String    -- reference word
          -> Model     -- model to train
          -> String    -- word to learn
          -> IO Model
trainWord alpha ref m word = do
  start <- getCurrentTime
--  putStr $ "Training on word " ++ word
  let h = dictionary $ vocabulary m
  let Just (Coding index _ huff points) = M.lookup ref h
  let Just (Coding index' _ huff' points') = M.lookup word h
  -- this a vector of encoding length columns
  inputLayer  <- subMatrix [index'] (syn0 m)
  -- this is a square subMatrix of n rows plus zeros, each modelSize column
  -- where n is the length of huffman encoding
  -- we complete to 0 in order to ensure that downstream computations are consistent as the
  -- original encoding yield fewer points for larger frequencies
  hiddenLayer <- squaredMatrix points (syn1 m) 
  -- this is a vector of encoding length columns
  let propagate = inputLayer `matrixProduct` (transpose hiddenLayer)
  let [one] = matrixFromList 1 (cols propagate) [1,1 .. ]
  -- this is a vector of encoding length columns
  let fa = 1.0 `divideScalar` (one `plus` matrixExp (one `minus` propagate))
  -- A matrix (actually a vector) of 0 and 1 built from the huffman encoding of the current
  -- word. The vector is completed with 0s to ensure it has a consistent number of columns
  let huffMatrix = toMatrix (cols propagate) huff
  -- this is again a vector of encoding length columns representing the error gradients
  -- time learning rate alpha
  let ga = (one `minus` huffMatrix `minus` fa) `scalarProduct` alpha
  -- this is a encoding length x modelSize matrix
  let prod = ga `outerProduct` inputLayer
  let hiddenToOutput =  (hiddenLayer `plus` prod)
  -- learn hidden -> output
  -- add the two matrices
  !syn1'<- writeMatrix (syn1 m) points hiddenToOutput
  -- learn input -> hidden
  let inputToHidden = (inputLayer `plus` (ga `matrixProduct` hiddenLayer))
  !syn0' <- writeMatrix (syn0 m) [index']  inputToHidden
  end <- getCurrentTime
--  putStrLn $ " in " ++ (show $ diffUTCTime end start)
  return $ m { syn0 = syn0', syn1 = syn1'} 


-- |Construct a model from a Dictionary
fromDictionary :: Dictionary -> IO Model
fromDictionary d@(Dict dict size len) = model size len >>= return . \ m -> m { vocabulary = d }

-- |Initializes a model of given size
--
-- The output connections are initialized to 0 while the hidden connections are
-- initialized to random values in the [-0.5,+0.5] interval, then divided by the number of
-- columns.
model :: Int        -- dimensions
      -> Int        -- number of words
      -> IO Model
model words dim = do
  s0 <- randomConnectionValues words dim
  s1 <- emptyMatrix (words, dim)
  return $ Model words dim s0 s1 emptyDictionary defaultWindow

-- |Initialize the connection matrix with random values.
--
-- Values are distributed in such a way that each cell is between -0.5 and 0.5 and
-- is further divided by the total number of cols in the row so that the sum of values in
-- a row is always between -0.5 and +0.5
--
randomConnectionValues :: Int       -- number of rows
                       -> Int       -- number of cols per row
                       -> IO Matrix -- initialized matrix 
randomConnectionValues rows cols = do
  g <- getStdGen
  matrixFromList rows cols (map (/fromIntegral cols) (randoms g))

randoms :: RandomGen g => g -> [ Double ]
randoms g = let (i,g') = random g
            in (i - 0.5) : randoms g'
  
