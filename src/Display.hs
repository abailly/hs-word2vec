-- | Display a model in 2D after PCA analysis.
--
-- Principal component analysis extracts from a matrix the "most significant dimensions",
-- that is the dimensions which have the higher correlations with other dimensions. Or at
-- least this is what I understood...
-- We extract the two main principal components from the feature matrix of a model and generate
-- a 2d picture of the most frequent words from the dictionary.
module Display(drawSelectedWords, pcaAnalysis) where

-- Module containing code for PCA computation
import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import           Data.Default.Class
import qualified Data.HashMap.Strict      as H
import           Graphics.Rendering.Chart
import           Model.Repa
import           Model.Types
import           PCA
import           Words.Dictionary


type WordPoints = H.HashMap String [Double]

-- | Compute 2D mapping of words from a model.
--
-- We first transform the syn0 values of model into a Matrix of doubles
-- then compute 2 first PCA from this matrix. The first 2 PCAs are zipped along with each corresponding
-- word from the vocabulary to produce a vector of tuples with coordinates
pcaAnalysis :: Model -> WordPoints
pcaAnalysis m =
  let matrix       = toMatrix (numberOfWords m) (modelSize m) (syn0 m)
      pcf          = pca''' 2 matrix fastPCA
      indexedWords = orderedWords (vocabulary m)
      pcs          = toLists pcf
      wordsAndVecs = H.fromList $ zip indexedWords pcs
  in wordsAndVecs

-- |Draw  a chart of the X most frequent words in a model using PCA dimensions.
drawSelectedWords :: WordPoints  -- ^Result of PCA analysis from model
                  -> [String]                  -- ^Selected words to plot
                  -> Renderable ()             -- ^The output from Chart
drawSelectedWords vectors selectedWords = let
  coord [x,y] = (x * 1000,y * 1000)
  coord e     = error $ "invalid coordinates for point " ++ show e

  points  = plot_points_style .~ filledCircles 2 (opaque red)
           $ plot_points_values .~ (map  (coord . (vectors H.!)) selectedWords)
           $ def

  labels = plot_annotation_values .~  [(x * 1000 + 0.01,y * 1000 + 0.01,l) |  (l,x:y:_) <- H.toList (H.filterWithKey (\ k _ -> k `elem` selectedWords) vectors) ]
    $ def

  layout = layout_title .~ "Words Vector Space"
           $ layout_plots .~ [toPlot points, toPlot labels]
           $ def
  in
   toRenderable layout

