{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Interface and core types describing a training model
module Model.Types where

import qualified Data.Aeson          as J
import           Data.Array.Repa     ((:.) (..), Array, DIM1, U, Z (..),
                                      computeP, foldP, fromListUnboxed, ix1,
                                      ix2, slice, sumP, toList, toUnboxed, (!),
                                      (*^), (+^))
import qualified Data.Array.Repa     as R
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap         as I
import qualified Data.Vector         as V
import           GHC.Generics
import           Huffman
import           Words.Dictionary

-- | Used for vector computations
type Vector = Array U DIM1 Double

-- | More efficient to update part of a map than a complete matrix
type Layer = I.IntMap Vector

instance J.ToJSON Vector where
  toJSON v = J.Array $ V.fromList $ map J.toJSON $ toList v

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
  } deriving (Show, Read, Generic)

instance J.ToJSON Model

-- | Compute similarity between two words
--
-- Uses the cosine similarity, eg. dot product between the two vectors. The vectors should be of
-- norm 1 and equal length.
similarity :: Model -> String -> String -> IO Double
similarity m u v = do
  u'<- coefficient m u
  v'<- coefficient m v
  vecU <- unitVector u'
  vecV <- unitVector v'
  sumP (vecU *^ vecV) >>= return.(!Z)
    where
      -- | Raw coefficients of given word
      --
      -- Returns an array of model size length containing the raw coefficients for the given word
      -- in the given model.
      coefficient :: Model -> String -> IO Vector
      coefficient m w = do
        let h = dictionary $ vocabulary m
        let Just (Coding wordIndex _ _ _) = M.lookup w h
        let s0 = syn0 m
        return $ s0 I.! wordIndex

      -- | Normalize given array to a vector of length 1.
      unitVector :: Vector -> IO Vector
      unitVector v = do
        s <- foldP (\ s x -> s + (x * x)) 0 v
        let norm = sqrt (s ! Z)
        computeP $ R.map (/ norm) v

defaultWindow :: Int
defaultWindow = 10

defaultFeatures :: Int
defaultFeatures = 100


-- | Output a layer (matrix) as a list of doubles concatenating all rows
layerToList :: Layer -> [Double]
layerToList = concatMap toList . I.elems
