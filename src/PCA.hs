-- | Compute PCA from a Matrix
--
-- http://www.cs.otago.ac.nz/cosc453/student_tutorials/principal_components.pdf
module PCA (
  -- * PCA Computations
  pcaSVD, pca', pca'', pca''',
  pcaNipals, fastPCA, fastPCA',
  -- * Conversions
  toMatrix, toLists
  ) where

import           Debug.Trace
import           Model.Types                  (Layer, layerToList)
import qualified Model.Types                  as T
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.NIPALS

type Vec = Vector Double
type Mat = Matrix Double


-- | Turn a Layer into a Matrix for purpose of PCA.
toMatrix :: Int -> Int -> T.Mat -> Mat
toMatrix r c = (r >< c) . layerToList

-----------------------------------------------------
-- * Standard (Full) PCA Computation
-- https://github.com/albertoruiz/hmatrix/blob/master/examples/pca1.hs

-- | Run *Principal Component Analysis* on given Matrix and returns requested number
-- of most significant dimensions.
-- creates the compression and decompression functions from the desired number of components
pcaSVD :: Int -> Mat -> (Mat, Vec, Vec -> Vec , Vec -> Vec)
pcaSVD n dataSet = (vp, m, encode,decode)
  where
    encode x = vp' #> (x - m)
    decode x = x <# vp' + m
    (m,c) = meanCov dataSet
    (_,v) = eigSH (trustSym c)
    vp = takeColumns n v
    vp' = tr vp

-- | Return a function that yields the PCA vector for some index of given matrix
pca' :: Int -> Mat -> (Int -> [Double])
pca' n dataSet = toList . enc . (mat' !!)
  where
    mat' = toRows dataSet
    (_,_,enc,_) = pcaSVD n dataSet

pca'' :: Int -> Mat -> (Int -> Mat -> Mat) -> Mat
pca'' n dataSet pca = tr (pcaMat <> tr dataSet)
  where
    pcaMat = pca n dataSet

pca''' :: Int -> Mat -> (Int -> Mat -> [Vec]) -> Mat
pca''' n dataSet pca = tr (pcaMat <> tr (dataSet - m'))
  where
    (m,c) = meanCov dataSet
    m' = fromRows $ replicate (rows dataSet) m
    -- pca returns a list of component vectors, e.g. columns
    pcaMat = fromRows $ pca n dataSet


--------------------------------------------------
-- * NIPALS Algorithm

pcaNipals :: Int -> Mat -> [ Vec ]
pcaNipals 0 _       = []
pcaNipals n dataSet = let (pc1, _ , residual) = firstPC dataSet
                      in  pc1 : pcaNipals (n - 1) residual

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

      max_iter = 30

      phi_p :: Vector Double
      phi_p = unitary $ konst 1 (cols dataSet)

      gram_schmidt :: Vector Double -> [ Vector Double ] -> Vector Double
      gram_schmidt phip phis = phip - sum (map (\ phi_j -> cmap (* (phip <.> phi_j)) phi_j) phis)

      go :: Vector Double -> Int -> Vector Double
      go phi k | k > max_iter = phi
               | otherwise    = let phi_p_new = cov #> phi
                                    norm_phi  = unitary $ gram_schmidt phi_p_new phis
                                    conv      = abs (norm_phi <.> phi - 1) < peps
                                in  if conv
                                    then norm_phi
                                    else go norm_phi (k+1)

      new_phi = go phi_p 0
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

