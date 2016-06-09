{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Interface and core types describing a training model
module Model.Types where

import           Control.Monad.Trans (MonadIO, liftIO)
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
import           System.Random
import           Words.Dictionary


class NN a where
  type Vector a :: *
  type Layer a :: *

  initialize :: (MonadIO m)
                => Int        -- number of words
                -> Int        -- number of features (dimensions)
                -> m a

  updateSinglePoint :: (MonadIO m)
                       => (Vector a, Layer a)
                       -> (Int, Bin)
                       -> m (Vector a, Layer a)

  similarity :: a -> String -> String -> IO Double

-- | Used for vector computations
type Vec = Array U DIM1 Double

-- | More efficient to update part of a map than a complete matrix
type Mat = I.IntMap Vec

instance J.ToJSON Vec where
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
  syn0          :: !Mat,

  -- The hidden -> output connection matrix
  -- It has the same geometry as the input layer.
  -- syn1 is the original name in C word2vec implementation
  syn1          :: !Mat,

  -- The dictionary
  -- Each word is mapped to a Coding structure containing, among other things,
  -- the Huffman encoding of the word and references to inner nodes this word is connected to
  -- contains also number of words and maximal length of coding vectors
  vocabulary    :: !Dictionary,

  -- Size of training window
  window        :: Int
  } deriving (Show, Read, Generic)

instance J.ToJSON Model


-- |Initializes a model of given size
--
-- The output connections are initialized to 0 while the hidden connections are
-- initialized to random values in the [-0.5,+0.5] interval, then divided by the number of
-- columns.
model :: (MonadIO m)
         => Int        -- number of words
         -> Int        -- number of features (dimensions)
         -> m Model
model numWords dim = do
  let wordsIndex = [0..numWords-1]
  vecs <- mapM (const $ liftIO $ randomVector dim) wordsIndex
  let s0 = I.fromList (zip wordsIndex vecs)
  let nulls = map (const $ R.fromListUnboxed (Z :. dim) (replicate dim 0)) wordsIndex
  let s1 = I.fromList (zip wordsIndex nulls)
  return $ Model numWords dim s0 s1 emptyDictionary defaultWindow
    where
      -- |Initialize a vector with random values.
      --
      -- Values are distributed in such a way that each cell is between -0.5 and 0.5 and
      -- is further divided by the total number of cells row so that the sum of values in
      -- a row is always between -0.5 and +0.5
      --
      randomVector :: Int       -- number of cells
                   -> IO Vec -- initialized vector
      randomVector cols = do
        g <- getStdGen
        return $ fromListUnboxed (Z :. cols) (take cols (map (/fromIntegral cols) (randoms g)))

      randoms :: RandomGen g => g -> [ Double ]
      randoms g = let (i,g') = random g
                  in (i - 0.5) : randoms g'

-- | Compute similarity between two words
--
-- Uses the cosine similarity, eg. dot product between the two vectors. The vectors should be of
-- norm 1 and equal length.
distance :: Model -> String -> String -> IO Double
distance m u v = do
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
      coefficient :: Model -> String -> IO Vec
      coefficient m w = do
        let h = dictionary $ vocabulary m
        let Just (Coding wordIndex _ _ _) = M.lookup w h
        let s0 = syn0 m
        return $ s0 I.! wordIndex

      -- | Normalize given array to a vector of length 1.
      unitVector :: Vec -> IO Vec
      unitVector v = do
        s <- foldP (\ s x -> s + (x * x)) 0 v
        let norm = sqrt (s ! Z)
        computeP $ R.map (/ norm) v


-- |Update a single row of a matrix with a vector at given index.
updateLayer :: Vec -> Int -> Mat -> Mat
updateLayer v = I.adjust (const v)

-- | An initialised vector to be used for computing single update of a word
initialVector :: Model -> Vec
initialVector m = fromListUnboxed (ix1 layerSize) (replicate layerSize 0)
  where
    layerSize = modelSize m


defaultWindow :: Int
defaultWindow = 10

defaultFeatures :: Int
defaultFeatures = 100


-- | Output a layer (matrix) as a list of doubles concatenating all rows
layerToList :: Mat -> [Double]
layerToList = concatMap toList . I.elems
