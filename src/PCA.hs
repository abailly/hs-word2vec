-- | Compute PCA from a Matrix
--
-- http://www.cs.otago.ac.nz/cosc453/student_tutorials/principal_components.pdf
module PCA where

import           Debug.Trace
import           Model.Types                  (Layer, layerToList)
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.NIPALS

type Vec = Vector Double
type Mat = Matrix Double


-- | Turn a Layer into a (transposed) Matrix for purpose of PCA.
toMatrix :: Int -> Int -> Layer -> Mat
toMatrix r c = tr . (r >< c) . layerToList


--------------------------------------------------
-- * NIPALS Algorithm

pcaNipals :: Int -> Mat -> [ Vec ]
pcaNipals 0 _       = []
pcaNipals n dataSet = let (pc1, _ , residual) = firstPC dataSet
                      in  pc1 : pcaNipals (n - 1) residual

-----------------------------------------------------
-- * Standard (Full) PCA Computation
-- https://github.com/albertoruiz/hmatrix/blob/master/examples/pca1.hs

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
fastPCA n dataSet = fastPCARec n dataSet []

fastPCARec :: Int -> Matrix Double -> [ Vector Double ] -> [ Vector Double ]
fastPCARec 0 _       _ = []
fastPCARec n dataSet phis =
  let (_,cov) = meanCov dataSet       -- compute covariance matrix

      phi_p :: Vector Double
      phi_p = unitary $ konst 1 (cols dataSet)

      gram_schmidt :: Vector Double -> [ Vector Double ] -> Vector Double
      gram_schmidt phip phis = phip - sum (map (\ phi_j -> cmap (* (phip <.> phi_j)) phi_j) phis)

      go :: Vector Double -> Vector Double
      go phi = let phi_p_new = cov #> phi
                   norm_phi  = unitary $ gram_schmidt phi_p_new phis
                   conv      = abs (norm_phi <.> phi - 1) < peps
               in  if conv
                   then norm_phi
                   else go norm_phi

      new_phi = go phi_p
  in new_phi : fastPCARec (n-1) dataSet (new_phi:phis)


----------------------------------------------
-- * Another Fast PCA algorithm
-- Computes top k principal components using power iteration method
-- http://theory.stanford.edu/~tim/s15/l/l8.pdf

fastPCA' :: Int -> Matrix Double -> [ Vector Double ]
fastPCA' n dataSet = fastPCARec' n seed
  where
    square = uncurry (==) . size

    seed = if not (square dataSet)
           then tr dataSet <> dataSet
           else dataSet

    fastPCARec' 0 _        = []
    fastPCARec' k mat  =
      let v_0   = unitary $ konst 1 (cols mat)
          go v  = let v'     = mat #> v
                      unit_v = unitary v'
                      stop   = abs (unit_v <.> unitary v - 1) < peps
                  in if stop
                     then unit_v
                     else go v'
          new_v = go v_0
          mat_v = mat #> new_v
          mat'  = mat - (mat_v `outer` new_v)
      in new_v : fastPCARec' (k-1) mat'

