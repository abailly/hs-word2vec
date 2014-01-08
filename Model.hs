{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- |Neural network based model of words similarity
module Model where
import Control.Monad(foldM)
import System.Random(random,randomR,getStdGen,RandomGen,mkStdGen)
import qualified Data.HashMap.Strict as M
import Matrix
import Huffman
import Window

data Model = Model {
  -- Size of the model or number of dimensions each word is mapped to
  modelSize :: Int,
  
  -- Number of words in the model
  numberOfWords :: Int,
  
  -- The input -> hidden connection matrix
  -- input layer has size equal to number of words in vocabulary, with each
  -- cell connected to a number of hidden cells equal to the 'dimension' of the model
  -- eg, the number of features we want to track defaulting to 100
  --
  -- syn0 is the original name in C word2vec implementation
  syn0 :: Matrix,

  -- The hidden -> output connection matrix
  --
  -- syn1 is the original name in C word2vec implementation
  syn1 :: Matrix,

  -- The vocabulary
  -- Each word is mapped to a Coding structure containing, among other things,
  -- the Huffman encoding of the word and references to inner nodes this word is connected to
  vocabulary :: M.HashMap String Coding,

  -- Size of training window
  window :: Int
  }

defaultWindow :: Int
defaultWindow = 10

-- |Train given model with a single sentence.
--
-- This function updates the model using skip-gram hierarchical softmax model on a single sentence
-- For each word in the sentence:
--
-- * Define a random training window around the word
-- * Iterate over all words in the window, updating the underlying neural network 
trainSentence :: Model
              -> [String]
              -> Double
              -> IO Model
trainSentence m sentence alpha = do
  g <- getStdGen
  foldM (trainWindow alpha) m (slidingWindows (window m) g sentence)

      
trainWindow :: Double              -- alpha threshold 
          -> Model               -- model to train
          -> (String,[String])   -- prefix, word, suffix to select window around word
          -> IO Model            -- updated model
trainWindow alpha m (word,words) =
  foldM (trainWord alpha word) m (filter (/= word) words)

trainWord :: Double    -- alpha threshold 
          -> String    -- reference word
          -> Model     -- model to train
          -> String    -- word to learn
          -> IO Model
trainWord alpha ref m word = do
  let h = vocabulary m
  let Just (Coding index _ huff points) = M.lookup ref h
  let Just (Coding index' _ huff' points') = M.lookup word h
  -- this a vector of modelSize columns
  inputLayer  <- subMatrix [index'] (syn0 m) 
  -- this is a subMatrix of n rows each modelSize column
  -- where n is the length of huffman encoding
  hiddenLayer <- subMatrix points (syn1 m) 
  -- this is a vector of encoding length columns
  let propagate = inputLayer `matrixProduct` (transpose hiddenLayer)
  let [one] = matrixFromList 1 (cols propagate) [1,1 .. ]
  -- this is a vector of encoding length columns
  let fa = 1.0 `divideScalar` (one `plus` matrixExp (one `minus` propagate))
  -- this is again a vector of encoding length columns representing the error gradients
  -- time learning rate alpha
  let ga = (one `minus` toMatrix huff `minus` fa) `scalarProduct` alpha
  -- this is a encoding length x modelSize matrix
  let prod = ga `outerProduct` inputLayer
  -- learn hidden -> output
  -- add the two matrices
  syn1'<- writeMatrix (syn1 m) points (hiddenLayer `plus` prod)
  -- learn input -> hidden
  syn0' <- writeMatrix (syn0 m) [index'] (inputLayer `plus` (ga `matrixProduct` hiddenLayer))
  return $ m { syn0 = syn0', syn1 = syn1'} 

  
          
-- |Initializes a model of given size
--
-- The output connections are initialized to 0 while the hidden connections are
-- initialized to random values in the [-0.5,+0.5] interval.
model :: Int        -- dimensions
      -> Int        -- number of words
      -> IO Model
model dim words = do
  s0 <- randomConnectionValues words dim
  s1 <- emptyMatrix (words, dim)
  return $ Model dim words s0 s1 M.empty defaultWindow

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
  -- TODO divide each row's values by cols
  matrixFromList rows cols (randoms g)

randoms :: RandomGen g => g -> [ Double ]
randoms g = let (i,g') = random g
            in (i - 0.5) : randoms g'
  
