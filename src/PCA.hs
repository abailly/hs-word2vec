-- | Compute PCA from a Matrix
--
-- https://github.com/albertoruiz/hmatrix/blob/master/examples/pca1.hs
-- http://www.cs.otago.ac.nz/cosc453/student_tutorials/principal_components.pdf
module PCA where

import           Model.Types           (Layer, layerToList)
import           Numeric.LinearAlgebra

type Vec = Vector Double
type Mat = Matrix Double


-- | Turn a Layer into a (transposed) Matrix for purpose of PCA.
toMatrix :: Int -> Int -> Layer -> Mat
toMatrix r c = tr . (r >< c) . layerToList

-- | Run *Principal Component Analysis* on given Matrix and returns requested number
-- of most significant dimensions.
-- creates the compression and decompression functions from the desired number of components
pca :: Int -> Mat -> (Mat, Vec, Vec -> Vec , Vec -> Vec)
pca n dataSet = (vp, m, encode,decode)
  where
    encode x = vp #> (x - m)
    decode x = x <# vp + m
    (m,c) = meanCov dataSet
    (_,v) = eigSH (trustSym c)
    vp = tr $ takeColumns n v

-- | Return a function that yields the PCA vector for some index of given matrix
pca' :: Int -> Mat -> (Int -> [Double])
pca' n dataSet = toList . enc . (mat' !!)
  where
    mat' = toRows dataSet
    (_,_,enc,_) = pca n dataSet

pca'' :: Int -> Mat -> Mat
pca'' n dataSet = tr (pcaMat <> tr dataSet)
  where
    (pcaMat,_,_,_) = pca n dataSet
