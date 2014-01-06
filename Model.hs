{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- |Neural network based model of words similarity
module Model where
import Control.Monad(foldM)
import System.Random(random,randomR,getStdGen,RandomGen,mkStdGen)
import Data.HashMap.Strict(HashMap,empty)
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
  vocabulary :: HashMap String Coding,

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
  foldM (trainWord alpha) m (slidingWindows (window m) g sentence)



      
trainWord :: Double              -- alpha threshold 
          -> Model               -- model to train
          -> (String,[String])   -- prefix, word, suffix to select window around word
          -> IO Model            -- updated model
trainWord alpha m (word,words) = do
  
  return m
  
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
  return $ Model dim words s0 s1 empty defaultWindow

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
  m <- emptyMatrix (rows,cols)
  foldMatrix ( \ g _ _ _ ->
                let (i,g') = random g 
                in (g',(i- 0.5) / fromIntegral cols)) g m >>= return.snd
  
