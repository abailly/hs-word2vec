-- |Provide simple 2D matrices
module Matrix(Matrix,
              emptyMatrix,
              matrix,
              printMatrix,
              updateMatrix,
              subMatrix,
              matrixProduct, outerProduct, plus,
              writeMatrix,
              transpose,
              matrixFromList
             ) where
import Control.Monad(mapM, forM, forM_, foldM, liftM3)
import Data.Array.IO(
  IOUArray,
  getElems,
  newArray,
  readArray,
  writeArray)
import qualified Data.Packed.Matrix as M
import Data.List(intersperse)

-- | Define a 2D Matrix stored in IO as a single dimensional array
data Matrix = Matrix {
  -- The raw (Mutable) array of data
  rawData :: M.Matrix Double,
  -- Number of rows
  rows :: Int,
  -- Number of columns
  cols :: Int
  }

-- |Select a part (view) of a Matrix
--
-- The returned matrix is mutable and tied to the original matrix, so modifying it
-- will modify the input matrix'data
subMatrix :: Matrix      -- input matrix
          -> [Int]       -- a list of row indices to select
          -> IO Matrix   -- sub-matrix from input
subMatrix m _ = return m

-- |Product of two matrices
--
matrixProduct :: Matrix -> Matrix -> Matrix
matrixProduct m n = m

-- |Sum of two matrices
--
plus :: Matrix -> Matrix -> Matrix
plus m n = m

-- |Transposition of matrices
transpose :: Matrix -> Matrix
transpose m = m

-- |Outer product of two vectors
outerProduct :: Matrix -> Matrix -> Matrix
outerProduct m n = m `matrixProduct` (transpose n)

writeMatrix :: Matrix -> Matrix -> IO ()
writeMatrix m n = return ()

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
  return $ Matrix a c r

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
      
