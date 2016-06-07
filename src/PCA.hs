-- | Compute PCA from a Matrix
--
-- http://www.cs.otago.ac.nz/cosc453/student_tutorials/principal_components.pdf
module PCA where

import           Debug.Trace
import           Model.Types           (Layer, layerToList)
import           Numeric.LinearAlgebra

type Vec = Vector Double
type Mat = Matrix Double


-----------------------------------------------------
-- * Standard (Full) PCA Computation
-- https://github.com/albertoruiz/hmatrix/blob/master/examples/pca1.hs

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


-----------------------------------------------------
-- * Fast (Iterative) PCA Computation
-- https://maxwell.ict.griffith.edu.au/spl/publications/papers/prl07_alok_pca.pdf

-- | Computes a list of top PCA components for given matrix
fastPCA :: Int -> Matrix Double -> [ Vector Double ]
fastPCA n dataSet = let (_,cov) = meanCov dataSet       -- compute covariance matrix
                        p = 1

                        phi_p :: Vector Double
                        phi_p = unitary $ konst 1 (cols dataSet)

                        gram_schmidt :: Vector Double -> [ Vector Double ] -> Vector Double
                        gram_schmidt phip phis = let gs = phip - sum (map (\ phi_j -> cmap (* (phip <.> phi_j)) phi_j) phis)
                                                 in trace ("gram-Schmidt:  " ++ show gs) $ gs

                        go :: [ Vector Double ] -> Vector Double
                        go (phi:phis) = let phi_p_new = trace ("phi: " ++ show phi) $ cov #> phi
                                            norm_phi  = trace ("phi_p_new: " ++ show phi_p_new) $ unitary $ gram_schmidt phi_p_new phis
                                            conv      = trace ("norm_phi: " ++ show norm_phi) $ abs (norm_phi <.> phi - 1) < peps
                                        in  if conv
                                            then norm_phi
                                            else go (norm_phi:phis)
                    in [go [phi_p]]


----------------------------------------------
-- * Another Fast PCA algorithm
-- Computes top k principal components using power iteration method
-- http://theory.stanford.edu/~tim/s15/l/l8.pdf

fastPCA' :: Int -> Matrix Double -> [ Vector Double ]
fastPCA' n dataSet = let seed  = tr dataSet <> dataSet
                         v_0  = unitary $ konst 1 (cols dataSet)
                         go v = let v'     = seed #> v
                                    norm_v = unitary v'
                                    stop   = abs (norm_v <.> unitary v - 1) < peps
                                in if stop
                                   then norm_v
                                   else go v'
                     in [go v_0]
