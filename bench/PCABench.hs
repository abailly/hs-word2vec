module PCABench where

import           Numeric.LinearAlgebra
import           PCA

computePCA :: Int -> IO (Matrix Double)
computePCA n = do
  mat <- randn n n
  return $ pca'' 2 mat
