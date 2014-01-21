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
import Huffman
import Window
import Words

type Layer = A.IOUArray Int Double

instance Show Layer where
  -- TODO this is bad...refactor me
  show l = show $ unsafePerformIO $ A.getElems l
    
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

-- | Raw coefficients of given word
--
-- Returns an array of model size length containing the raw coefficients for the given word
-- in the given model.
coefficient :: Model -> String -> IO Layer
coefficient m w = do
  let h = dictionary $ vocabulary m
  let Just (Coding wordIndex _ _ _) = M.lookup w h
  let layerSize = modelSize m
  let offset = wordIndex * layerSize
  let s0 = syn0 m
  coeff <- A.newArray (0, layerSize -1) 0 :: IO Layer
  mapM_ (\ c -> A.readArray s0 (c + offset) >>= A.writeArray coeff c) [0 .. layerSize - 1]
  return coeff

-- | Normalize given array to a vector of length 1.
unitVector :: Layer -> IO Layer
unitVector v = do
  (lb,ub) <- A.getBounds v
  s <- foldM (\ s c -> A.readArray v c >>= (\ x -> return (s + (x * x)))) 0 [lb .. ub]
  let norm = sqrt s
  A.mapArray (/ norm) v
  
  
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
  (lb,ub) <- A.getBounds vecU
  foldM ( \ s c -> A.readArray vecU c >>=
                   (\ x -> A.readArray vecV c >>=
                           (\ y -> return (s + x * y)))) 0 [lb .. ub]

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
  let layerIndices = [0..layerSize -1]

  let s0 = syn0 m
  let s1 = syn1 m

  -- temporary arrays
  neu1e <- A.newArray (0, layerSize - 1) 0 :: IO Layer

  -- offset in input layer for word to learn features
  let l1 = index' * layerSize

  -- update a single point
  let updatePoint (p,b) = do
        -- offset for link to index p in hidden layer
        let l2 = p * layerSize
        -- dot product of two vectors
        f <- foldM (\ f' c -> liftM2 (*) (A.readArray s0 (c + l1)) (A.readArray s1 (c + l2)) >>= \ v -> return (v + f')) 0 layerIndices
        let exp_f = exp f
        -- compute gradient
        let g = (1 - asNum b - exp_f) * alpha
        -- apply gradient on input layer
        for layerSize s1 (+l2) neu1e id neu1e id (\ x y  -> y + g * x)
        -- apply gradien on hidden layer
        for layerSize s0 (+l1) s1 (+l2) s1 (+l2) (\ x y  -> y + g * x)

  mapM_ updatePoint (zip points huff)

  -- report computed gradient to input layer
  for layerSize s0 (+l1) neu1e id s0 (+l1) (+)
  
  -- not very useful given we update arrays in place, but simplifies folding function over a list of words
  return $ m

for :: Int          -- ^Size of array
    -> Layer        -- ^Left argument
    -> (Int -> Int)  -- ^First array offset
    -> Layer        -- ^Right Argument
    -> (Int -> Int)  -- ^Second array offset
    -> Layer        -- ^Output Array
    -> (Int -> Int)  -- ^Output offset
    -> (Double -> Double -> Double)  -- ^Computed function. The arguments are drawn from first and second array 
    -> IO ()
for size a offa b offb r offr f =
  mapM_ (\ c -> A.readArray a (offa c) >>=
        (\ h -> A.readArray b (offb c) >>=
        (\ n -> A.writeArray r (offr c) (f h n))))
        [0 .. size - 1]

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
  s0 <- randomConnectionValues words dim
  s1 <- A.newArray (0, words * dim -1) 0
  return $ Model words dim s0 s1 emptyDictionary defaultWindow

-- |Initialize the connection matrix with random values.
--
-- Values are distributed in such a way that each cell is between -0.5 and 0.5 and
-- is further divided by the total number of cols in the row so that the sum of values in
-- a row is always between -0.5 and +0.5
--
randomConnectionValues :: Int       -- number of rows
                       -> Int       -- number of cols per row
                       -> IO Layer -- initialized matrix 
randomConnectionValues rows cols = do
  g <- getStdGen
  A.newListArray (0,rows * cols -1) (map (/fromIntegral cols) (randoms g))
  

randoms :: RandomGen g => g -> [ Double ]
randoms g = let (i,g') = random g
            in (i - 0.5) : randoms g'
  
