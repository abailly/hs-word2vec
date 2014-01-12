-- |Provide simple 2D matrices
module Matrix(Matrix(..),
              emptyMatrix,
              matrix,
              printMatrix,
              updateMatrix,
              subMatrix,
              squaredMatrix,
              matrixProduct, outerProduct,
              scalarProduct,
              dotProduct,
              divideScalar,
              matrixExp,
              plus,
              minus,
              writeMatrix,
              transpose,
              matrixFromList,
              unitVector
             ) where
import Control.Monad(mapM, forM, forM_, foldM, liftM3)
import Data.Array.IO(
  IOUArray,
  getElems,
  newArray,
  readArray,
  writeArray)
import qualified Numeric.LinearAlgebra as M
import Data.List(intersperse,
                 mapAccumL)

-- | Define a 2D Matrix stored in IO as a single dimensional array
data Matrix = Matrix {
  -- The raw (Mutable) array of data
  rawData :: !(M.Matrix Double),
  -- Number of rows
  rows :: !Int,
  -- Number of columns
  cols :: !Int
  } deriving (Show)

-- |Select a part (view) of a Matrix
--
-- >>> (matrixFromList 3 3 (map fromIntegral [0 ..])) >>= subMatrix [0,2] >>= printMatrix >>= putStrLn
-- [0.0,1.0,2.0]
-- [6.0,7.0,8.0]  
subMatrix :: (Monad m) => [Int]       -- a list of row indices to select
          -> Matrix      -- input matrix
          -> m Matrix   -- sub-matrix from input
subMatrix rows m = return $ Matrix (M.extractRows rows (rawData m)) (length rows) (cols m)

-- |Select some rows of a matrix and complete them to ensure the matrix is square.
--
-- This assumes the selection has less rows than columns
-- 
-- >>> (matrixFromList 5 3 (map fromIntegral [0 ..])) >>= squaredMatrix [0,2] >>= printMatrix >>= putStrLn
-- [0.0,1.0,2.0]
-- [6.0,7.0,8.0]  
-- [0.0,0.0,0.0]
--
-- >>> (matrixFromList 5 4 (map fromIntegral [0 ..])) >>= squaredMatrix [0,2] >>= printMatrix >>= putStrLn
-- [0.0,1.0,2.0,3.0]
-- [8.0,9.0,10.0,11.0]  
-- [0.0,0.0,0.0,0.0]
-- [0.0,0.0,0.0,0.0]
--
-- >>> (matrixFromList 5 3 (map fromIntegral [0 ..])) >>= squaredMatrix [0,2,4] >>= printMatrix >>= putStrLn
-- [0.0,1.0,2.0]
-- [6.0,7.0,8.0]  
-- [12.0,13.0,14.0]
squaredMatrix :: (Monad m) => [Int]       -- a list of row indices to select
          -> Matrix      -- input matrix
          -> m Matrix   -- sub-matrix from input
squaredMatrix rows m = let missing = cols m - length rows
                           extracted = M.extractRows rows (rawData m)
                           square = if missing > 0 then
                                      (M.fromBlocks $ [[extracted]] ++ replicate missing [0])
                                    else
                                      extracted
                       in return $ Matrix square (M.rows square) (cols m)

-- |Product of two matrices
matrixProduct :: Matrix -> Matrix -> Matrix
matrixProduct m n = Matrix  (rawData m M.<> rawData n) (rows m) (cols n)

scalarProduct :: Matrix -> Double -> Matrix
scalarProduct m x = m {rawData = M.mapMatrix (* x) (rawData m ) }

divideScalar :: Double -> Matrix -> Matrix
divideScalar x m = m {rawData = M.mapMatrix (x /) (rawData m ) }

matrixExp :: Matrix -> Matrix
matrixExp m = m { rawData = exp (rawData m) }

-- |Sum of two matrices
plus :: Matrix -> Matrix -> Matrix
plus m n = Matrix (rawData m + rawData n) (rows m) (cols m)

-- |Difference of two matrices
minus :: Matrix -> Matrix -> Matrix
minus m n = Matrix (rawData m - rawData n) (rows m) (cols m)

-- |Transposition of matrices
transpose :: Matrix -> Matrix
transpose m = Matrix (M.trans $ rawData m) (cols m) (rows m)

-- |Outer product of two vectors
--
-- >>> matrix [[1,1,1,1]] >>= (\ m -> matrix [[1,1,1,1]] >>= return.outerProduct m) >>= printMatrix >>= putStrLn
-- [4.0]
outerProduct :: Matrix -> Matrix -> Matrix
outerProduct m n = m `matrixProduct` (transpose n)

-- | Inner product of two vectors
--
-- >>> matrix [[0,1]] >>= (\ m -> matrix [[0,1]] >>= return.dotProduct m) >>= putStrLn.show
-- 1.0
--
-- >>> matrix [[1,1]] >>= (\ m -> matrix [[-1,1]] >>= return.dotProduct m) >>= putStrLn.show
-- 0.0
dotProduct :: Matrix -> Matrix -> Double
dotProduct m n = let m' = M.flatten $ rawData m
                     n' = M.flatten $ rawData n
                 in  m' M.<.> n'

-- | Normalize a vector to unit length
--
-- Assume number of rows is one...
-- >>> matrix [[2,0]] >>= return. unitVector >>= printMatrix >>= putStrLn
-- [1.0,0.0]
-- >>> matrix [[0,0],[0,2]] >>= return. unitVector >>= printMatrix >>= putStrLn
-- [0.0,0.0]
-- [0.0,1.0]
unitVector :: Matrix -> Matrix
unitVector m = let m' = rawData m
                   v = M.flatten m'
                   norm = sqrt $ v M.<.> v 
               in Matrix (M.mapMatrix (/ norm) m') (rows m) (cols m)
  
-- | Overwrite the given rows of a matrix with the last argument.
--
writeMatrix :: (Monad m) => Matrix -> [Int] -> Matrix -> m Matrix
writeMatrix m indices n = do
  let updatedRows = zip indices (M.toRows (rawData n))
  let accum (idx,upd) actual =  if (upd /= []) && fst (head upd)  == idx then
                                  ((idx + 1, tail upd), snd (head upd))
                                else
                                  ((idx + 1, upd), actual)
  return $ Matrix (M.fromRows (snd $ mapAccumL accum (0,updatedRows) (M.toRows (rawData m)))) (rows m) (cols n)
  

-- |Iterate over all cells of a Matrix, row by row and col by col, applying side effect
--
-- >>> matrix [[0,1],[2,3]] >>= iterateMatrix_  (\ r c e -> putStrLn $ show r ++ "," ++ show c ++ " = " ++ show e) 
-- 0,0 = 0.0
-- 0,1 = 1.0
-- 1,0 = 2.0
-- 1,1 = 3.0
iterateMatrix_ :: (Int -> Int -> Double -> IO ()) -> Matrix -> IO ()
iterateMatrix_ f m = M.mapMatrixWithIndexM_ (uncurry f) (rawData m)

-- |Map a value over all cells in a matrix and update it.
--
-- This function mutates the given matrix
--
-- >>> matrix [[0,1],[2,3]] >>= return. (updateMatrix  (\ r c e -> 2 * e))  >>= printMatrix >>= putStrLn
-- [0.0,2.0]
-- [4.0,6.0]  
updateMatrix :: (Int -> Int -> Double -> Double) -> Matrix -> Matrix
updateMatrix f m = Matrix (M.mapMatrixWithIndex (uncurry f) d) r c
  where
    r = rows m
    c = cols m
    d = rawData m

-- |Build a matrix of given cols and rows from a list of values
--
-- >>> matrixFromList 2 3 (map fromIntegral [0 ..]) >>= printMatrix >>= putStrLn
-- [0.0,1.0,2.0]
-- [3.0,4.0,5.0]  
matrixFromList :: (Monad m) => Int -> Int -> [ Double ] -> m Matrix
matrixFromList rows cols values = return $ Matrix ((rows M.>< cols) values) rows cols

-- |An empty matrix of given number of (rows,cols)
--
-- Content of matrix is initialized to 0
emptyMatrix :: (Monad m) => (Int,Int) -> m Matrix
emptyMatrix (r,c) = do
  let a = (r M.>< c) [ 0, 0 ..]
  return $ Matrix a r c

-- |Make a mutable matrix from a list of list of double values
-- Assume matrix is well-formed, eg. all rows have equal number of arguments
matrix :: (Monad m) => [[ Double ]] -> m Matrix
matrix values = return $ Matrix m (M.rows m) (M.cols m)
  where m = M.fromLists values

-- |Print a nicely formatted matrix
--
-- >>> matrix [[0,1],[2,3]] >>= printMatrix >>= putStrLn
-- [0.0,1.0]
-- [2.0,3.0]
printMatrix :: Matrix -> IO String
printMatrix m = return $ concat $ intersperse "\n" $ map show (M.toLists (rawData m))
      
