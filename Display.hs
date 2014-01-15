-- | Display a model in 2D after PCA analysis.
--
-- Principal component analysis extracts from a matrix the "most significant dimensions",
-- that is the dimensions which have the higher correlations with other dimensions. Or at
-- least this is what I understood...
-- We extract the two main principal components from the feature matrix of a model and generate
-- a 2d picture of the most frequent words from the dictionary.
module Display where

import qualified Data.Array.IO as A
-- hmatrix package
import qualified Data.Packed.Matrix as M
import qualified Data.Packed.Vector as V
-- Module containing code for PCA computation
import qualified Numeric.LinearAlgebra.NIPALS as P

import Words
import Model


import Graphics.Rendering.Chart
import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Control.Lens

-- | Compute 2D mapping of words from a model.
--
-- We first transform the syn0 values of model into a Matrix of doubles
-- then compute 2 first PCA from this matrix. The 2PCA as zipped along with each corresponding
-- word from the vocabulary to produce a vector of tuples with coordinates
pcaAnalysis :: Model -> IO [(String, Double, Double)]
pcaAnalysis m = do
  matrix <- toMatrix (numberOfWords m) (modelSize m) (syn0 m)
  let (pc1, _ , residual) = P.firstPC matrix
  let (pc2, _ , _)        = P.firstPC residual
  let indexedWords        = orderedWords (vocabulary m)
  return $ zip3 indexedWords (V.toList pc1)  (V.toList pc2) 

-- |Draw  a chart of the X most frequent words in a model using PCA dimensions.
drawMostFrequentWords :: Int                       -- ^Limit number of words to display
                      -> Model                     -- ^The model to draw frequencies from
                      -> [(String,Double,Double)]  -- ^Result of PCA analysis from model
                      -> Renderable ()
drawMostFrequentWords limit model vectors = let
  points = plot_points_style .~ filledCircles 2 (opaque red)
           $ plot_points_values .~ [(x,y) |  (_,x,y) <- take 100 vectors]
           $ def

  layout = layout_title .~ "Words Vector Space"
           $ layout_plots .~ [toPlot points]
           $ def
  in
   toRenderable layout



toMatrix :: Int -> Int -> Layer -> IO (M.Matrix Double)
toMatrix r c l = A.getElems l >>= return . M.trans . (r M.>< c)
