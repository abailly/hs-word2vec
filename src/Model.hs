{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |Neural network based model of words similarity
module Model where

import           Control.Monad       (foldM)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Array.Repa     ((:.) (..), All (..), Any (..), Array,
                                      DIM1, DIM2, U, Z (..), computeP, foldP,
                                      fromListUnboxed, ix1, ix2, slice, sumP,
                                      (!), (*^), (+^))
import qualified Data.Array.Repa     as R
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap         as I
import           Data.Time.Clock     (diffUTCTime, getCurrentTime)
import           Huffman
import           Log
import           Model.Types
import           System.Random       (RandomGen, getStdGen, random)
import           Window
import           Words.Dictionary


-- | Train a model using a dictionary and a list of sentences
trainModel :: (Progress m) => Int -> Dictionary -> [[String]] -> m Model
trainModel numberOfFeatures dict sentences = do
  theModel <- fromDictionary numberOfFeatures dict
  let alpha = 0.001
  progress Middle $ StartTraining theModel
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
trainSentence :: (Progress m)
                 => Double
                 -> (Int, Model)
                 -> [String]
                 -> m (Int, Model)
trainSentence alpha (count,!m) sentence = do
  let len = length sentence
  start <- liftIO getCurrentTime
  progress Middle $ TrainingSentence count len
  g <- liftIO getStdGen
  m'<- foldM (trainWindow alpha) m (slidingWindows (window m) g sentence)
  end <- liftIO getCurrentTime
  progress Middle $ TrainedSentence (diffUTCTime end start)
  return (count + len,m')


trainWindow :: (Progress m)
               => Double            -- alpha threshold
               -> Model               -- model to train
               -> (String,[String])   -- prefix, word, suffix to select window around word
               -> m Model            -- updated model
trainWindow alpha !m (w, ws) = progress Fine (TrainingWindow alpha w ws) >>
                               foldM (trainWord alpha w) m (filter (/= w) ws)

-- | Train model on a single word, given a reference word
--
-- Uses skipgram training method to train model given two "close" words.
-- Code is a direct transposition of C code into Haskell using Mutable arrays.
trainWord :: (Progress m)
             => Double    -- alpha threshold
             -> String    -- reference word
             -> Model     -- model to train
             -> String    -- word to learn
             -> m Model
trainWord alpha ref m word = do
  progress Fine $ TrainWord word ref

  let h = dictionary $ vocabulary m
      Just (Coding _      _ huff points) = M.lookup ref h
      Just (Coding index' _ _    _     ) = M.lookup word h
      encodedPoints = zip points $ unCode huff

      s0 = syn0 m
      l0 = s0 I.! index'

  progress Fine $ InitialWordVector index' l0

  -- update a single point
  let updatePoint :: (Progress m)
                     => (Array U DIM1 Double, Mat)
                     -> (Int, Bin)
                     -> m (Array U DIM1 Double, Mat)
      updatePoint (neu1e,s1) (p,b) = do
        -- dot product of two vectors
        let l1 = s1 I.! p
        progress Fine $ BeforeUpdate p l1
        f <- sumP (l0 *^ l1)
        progress Fine $ DotProduct (R.linearIndex f 0)
        let exp_f = exp (f ! Z)
        -- compute gradient
        let g = (1 - asNum b - exp_f) * alpha
        progress Fine $ ErrorGradient g
        -- apply gradient on input layer
        neu1e' <- computeP $ (R.map (*g) l1) +^ neu1e
        progress Fine $ InputLayerAfterGradient neu1e'
        -- apply gradient on hidden layer
        l1' <- computeP $ (R.map (*g) l0) +^ l1
        progress Fine $ HiddenLayerAfterGradient l1'
        return (neu1e',updateLayer l1' p s1)


  (neu1e, s1')  <- foldM updatePoint (initialVector m, syn1 m) encodedPoints

  progress Fine $ UpdatedWordVector index' neu1e

  -- report computed gradient to input layer
  return $ m { syn0 = updateLayer neu1e index' s0, syn1 = s1' }

-- |Construct a model from a Dictionary
fromDictionary :: (MonadIO m) => Int -> Dictionary -> m Model
fromDictionary numberOfFeatures d@(Dict _ size _) = model size numberOfFeatures >>= return . \ m -> m { vocabulary = d }

mostFrequentWords :: Int -> Model -> [ String ]
mostFrequentWords len = take len . orderedWords . vocabulary

