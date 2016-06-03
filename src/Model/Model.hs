-- | Interface and core types describing a training model
module Model.Model where

import           Data.Array.Repa  ((:.) (..), Array, DIM1, U, Z (..), computeP,
                                   foldP, fromListUnboxed, ix1, ix2, slice,
                                   sumP, toList, (!), (*^), (+^))
import qualified Data.IntMap      as I
import           Words.Dictionary

-- | Used for vector computations
type Vector = Array U DIM1 Double

-- | More efficient to update part of a map than a complete matrix
type Layer = I.IntMap Vector

data Model = Model {
  -- Number of words in the model
  numberOfWords :: Int,

  -- Size of the model or number of dimensions each word is mapped to
  -- also called number of features
  modelSize     :: Int,

  -- The input -> hidden connection matrix
  -- input layer has size equal to number of words in vocabulary, with each
  -- cell connected to a number of hidden cells equal to the 'dimension' of the model
  -- eg, the number of features we want to track defaulting to 100
  --
  -- syn0 is the original name in C word2vec implementation
  syn0          :: !Layer,

  -- The hidden -> output connection matrix
  -- It has the same geometry as the input layer.
  -- syn1 is the original name in C word2vec implementation
  syn1          :: !Layer,

  -- The dictionary
  -- Each word is mapped to a Coding structure containing, among other things,
  -- the Huffman encoding of the word and references to inner nodes this word is connected to
  -- contains also number of words and maximal length of coding vectors
  vocabulary    :: !Dictionary,

  -- Size of training window
  window        :: Int
  } deriving (Show, Read)

defaultWindow :: Int
defaultWindow = 10

defaultFeatures :: Int
defaultFeatures = 100


-- | Output a layer (matrix) as a list of doubles concatenating all rows
layerToList :: Layer -> [Double]
layerToList = concat . map toList . I.elems
