module PCABench where

import           Model
import           Model.Repa
import           Model.Types
import           Numeric.LinearAlgebra
import           PCA

type NumberOfComponents = Int
type SizeOfMatrix = Int
type NumberOfRows = Int
type NumberOfColumns = Int

computePCA :: SizeOfMatrix -> IO (Matrix Double)
computePCA n = do
  mat <- randn n n
  let pca n m = pcaMat where
        (pcaMat, _, _,_) = pcaSVD n m
  return $ pca'' 2 mat pca


computePCASVD :: NumberOfComponents -> SizeOfMatrix -> IO (Matrix Double)
computePCASVD c n = do
  mat <- randn n n
  let pca n m = pcaMat where
        (pcaMat, _, _,_) = pcaSVD n m
  return $ pca'' c mat pca

computeFastPCA :: NumberOfComponents -> SizeOfMatrix -> IO (Matrix Double)
computeFastPCA c n = do
  mat <- randn n n
  let pca c m = pcaMat where
        pcaMat = fromRows $ fastPCA c m
  return $ pca'' c mat pca

computePCAAndConvertModel :: NumberOfRows -> NumberOfColumns -> IO (Matrix Double)
computePCAAndConvertModel n m = do
  mat <- syn0 <$> model n m
  let pca n m = pcaMat where
        (pcaMat, _, _,_) = pcaSVD n m
  return $ pca'' 2 (toMatrix n m mat) pca

