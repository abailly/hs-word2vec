-- | Display a model in 2D after PCA analysis.
--
-- Principal component analysis extracts from a matrix the "most significant dimensions",
-- that is the dimensions which have the higher correlations with other dimensions. Or at
-- least this is what I understood...
-- We extract the two main principal components from the feature matrix of a model and generate
-- a 2d picture of the most frequent words from the dictionary.
module Display(drawSelectedWords, pcaAnalysis) where

-- hmatrix package
import qualified Data.Packed.Matrix           as M
import qualified Data.Packed.Vector           as V
-- Module containing code for PCA computation
import qualified Numeric.LinearAlgebra.NIPALS as P

import           Model
import           Words


import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import           Data.Default.Class
import           Graphics.Rendering.Chart

-- | Compute 2D mapping of words from a model.
--
-- We first transform the syn0 values of model into a Matrix of doubles
-- then compute 2 first PCA from this matrix. The first 2 PCAs are zipped along with each corresponding
-- word from the vocabulary to produce a vector of tuples with coordinates
pcaAnalysis :: Model -> [(String, Double, Double)]
pcaAnalysis m =
  let matrix = toMatrix (numberOfWords m) (modelSize m) (syn0 m)
      (pc1, _ , residual) = P.firstPC matrix
      (pc2, _ , _)        = P.firstPC residual
      indexedWords        = orderedWords (vocabulary m)
  in zip3 indexedWords (V.toList pc1)  (V.toList pc2)

-- |Draw  a chart of the X most frequent words in a model using PCA dimensions.
drawSelectedWords :: [(String,Double,Double)]  -- ^Result of PCA analysis from model
                  -> [String]                  -- ^Selected words to plot
                  -> Renderable ()             -- ^The output from Chart
drawSelectedWords vectors selectedWords = let
  points = plot_points_style .~ filledCircles 2 (opaque red)
           $ plot_points_values .~ [(x * 1000,y * 1000) |  (l,x,y) <- vectors, l `elem` selectedWords]
           $ def


  labels = plot_annotation_values .~ [(x * 1000 + 0.01,y * 1000 + 0.01,l) |  (l,x,y) <- vectors, l `elem` selectedWords]
           $ def

  layout = layout_title .~ "Words Vector Space"
           $ layout_plots .~ [toPlot points, toPlot labels]
           $ def
  in
   toRenderable layout


-- | Turn a Layer into a (transposed) Matrix for purpose of PCA.
toMatrix :: Int -> Int -> Layer -> M.Matrix Double
toMatrix r c = M.trans . (r M.>< c) . layerToList
