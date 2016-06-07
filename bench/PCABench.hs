module PCABench where

import           Model
import           Model.Types
import           Numeric.LinearAlgebra
import           PCA

computePCA :: Int -> IO (Matrix Double)
computePCA n = do
  mat <- randn n n
  return $ pca'' 2 mat


computePCA' :: Int -> Int -> IO (Matrix Double)
computePCA' n m = do
  mat <- syn0 <$> model n m
  return $ pca'' 2 $ toMatrix n m mat
