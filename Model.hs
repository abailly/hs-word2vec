{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
-- |Neural network based model of words similarity
module Model where
import Control.Monad(foldM, liftM2)
import System.Random(random,getStdGen,RandomGen)
import Data.Time.Clock(getCurrentTime,
                       diffUTCTime)
import System.IO.Unsafe
import qualified Data.HashMap.Strict as M

import qualified Data.Array.IO as A

import Data.Array.Repa(Z(..),
                       computeP,
                       sumP,
                       slice,
                       foldP,
                       All(..),
                       (:.)(..),
                       Any(..),
                       (*^),(+^),
                       ix2, ix1, Array, (!), U, DIM1, DIM2, fromListUnboxed)

import qualified Data.Array.Repa as R
import qualified Data.IntMap as I
import Huffman
import Window
import Words

-- | Used for vector computations
type Vector = (Array U DIM1 Double)

-- | More efficient to update part of a map than a complete matrix
type Layer = I.IntMap Vector

data Model = Model {
  -- Number of words in the model
  numberOfWords :: Int,
  
  -- Size of the model or number of dimensions each word is mapped to
  -- also called number of features
  modelSize :: Int,
  
  -- The input -> hidden connection matrix
  -- input layer has size equal to number of words in vocabulary, with each
  -- cell connected to a number of hidden cells equal to the 'dimension' of the model
  -- eg, the number of features we want to track defaulting to 100
  --
  -- syn0 is the original name in C word2vec implementation
  syn0 :: !Layer,

  -- The hidden -> output connection matrix
  -- It has the same geometry as the input layer.
  -- syn1 is the original name in C word2vec implementation
  syn1 :: !Layer,

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

defaultFeatures :: Int
defaultFeatures = 100

-- | Output a layer (matrix) as a list of doubles concatenating all rows
layerToList :: Layer -> [Double]
layerToList = concat . map R.toList . I.elems

-- | Raw coefficients of given word
--
-- Returns an array of model size length containing the raw coefficients for the given word
-- in the given model.
coefficient :: Model -> String -> IO Vector
coefficient m w = do
  let h = dictionary $ vocabulary m
  let Just (Coding wordIndex _ _ _) = M.lookup w h
  let layerSize = modelSize m
  let offset = wordIndex * layerSize
  let s0 = syn0 m
  return $ s0 I.! wordIndex

-- | Normalize given array to a vector of length 1.
unitVector :: Vector -> IO Vector
unitVector v = do
  s <- foldP (\ s x -> s + (x * x)) 0 v
  let norm = sqrt (s ! Z)
  computeP $ R.map (/ norm) v
  
  
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

-- | Train a model using a dictionary and a list of sentences
trainModel :: Int -> Dictionary -> [[String]] -> IO Model
trainModel _ dict sentences = do
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

      
trainWindow :: Double            -- alpha threshold 
          -> Model               -- model to train
          -> (String,[String])   -- prefix, word, suffix to select window around word
          -> IO Model            -- updated model
trainWindow alpha !m (w, ws) = 
  foldM (trainWord alpha w) m (filter (/= w) ws)

-- |Update a single row of a matrix with a vector at given index.
updateLayer :: Vector -> Int -> Layer -> Layer
updateLayer v x = I.adjust (const v) x 

-- | Train model on a single word, given a reference word
--
-- Uses skipgram training method to train model given two "close" words.
-- Code is a direct transposition of C code into Haskell using Mutable arrays.
trainWord :: Double    -- alpha threshold 
          -> String    -- reference word
          -> Model     -- model to train
          -> String    -- word to learn
          -> IO Model
trainWord alpha ref m word = do
  let h = dictionary $ vocabulary m
  let Just (Coding _ _ huff points) = M.lookup ref h
  let Just (Coding index' _ _ _) = M.lookup word h
  let layerSize = modelSize m
  let vocabSize = numberOfWords m
  let layerIndices = [0..layerSize -1]

  let s0 = syn0 m

  let neu1eInitial = fromListUnboxed (ix1 layerSize) (take layerSize [0..])
      
  let l0 = s0 I.! index'

  -- update a single point
      
  let updatePoint :: (Array U DIM1 Double, Layer) ->
                     (Int, Bin) ->
                     IO (Array U DIM1 Double, Layer)
      updatePoint (neu1e,s1) (p,b) = do
        -- dot product of two vectors
        let l1 = s1 I.! p
        f <- sumP (l0 *^ l1)
        let exp_f = exp (f ! Z)
        -- compute gradient
        let g = (1 - asNum b - exp_f) * alpha
        -- apply gradient on input layer
        neu1e' <- computeP $ (R.map (*g) l1) +^ neu1e
        -- apply gradient on hidden layer
        l1' <- computeP $ (R.map (*g) l0) +^ l1
        return (neu1e',updateLayer l1' p s1) 

      
  (neu1e, s1')  <- foldM updatePoint (neu1eInitial, syn1 m) (zip points huff)

  -- report computed gradient to input layer
  return $ m { syn0 = updateLayer neu1e index' s0, syn1 = s1' }

-- |Construct a model from a Dictionary
fromDictionary :: Dictionary -> IO Model
fromDictionary d@(Dict dict size len) = model size defaultFeatures >>= return . \ m -> m { vocabulary = d }

-- |Initializes a model of given size
--
-- The output connections are initialized to 0 while the hidden connections are
-- initialized to random values in the [-0.5,+0.5] interval, then divided by the number of
-- columns.
model :: Int        -- number of words
      -> Int        -- number of features (dimensions)
      -> IO Model
model words dim = do
  vecs <- mapM (const $ randomVector dim) [0..words-1]
  let s0 = I.fromList (zip [0..words-1] vecs)
  let nulls = map (const $ R.fromListUnboxed (Z :. dim) (take dim [0..])) [0..words-1]
  let s1 = I.fromList (zip [0..words-1] nulls)
  return $ Model words dim s0 s1 emptyDictionary defaultWindow

-- |Initialize a vector with random values.
--
-- Values are distributed in such a way that each cell is between -0.5 and 0.5 and
-- is further divided by the total number of cells row so that the sum of values in
-- a row is always between -0.5 and +0.5
--
randomVector :: Int       -- number of cells
             -> IO Vector -- initialized vector
randomVector cols = do
  g <- getStdGen
  return $ fromListUnboxed (Z :. cols) (take cols (map (/fromIntegral cols) (randoms g)))

randoms :: RandomGen g => g -> [ Double ]
randoms g = let (i,g') = random g
            in (i - 0.5) : randoms g'
  
