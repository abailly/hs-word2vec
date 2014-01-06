-- |Provide simple 2D matrices
module Matrix(Matrix,
              emptyMatrix,
              matrix,
              printMatrix,
              foldMatrix) where
import Control.Monad(mapM, forM, forM_, foldM, liftM3)
import Data.Array.IO(
  IOUArray,
  getElems,
  newArray,
  readArray,
  writeArray)
import Data.List(intersperse)

-- | Define a 2D Matrix stored in IO as a single dimensional array
data Matrix = Matrix {
  -- The raw (Mutable) array of data
  rawData :: IOUArray Int Double,
  -- Number of rows
  rows :: Int,
  -- Number of columns
  cols :: Int
  } deriving (Eq)


-- |Iterate over all cells of a Matrix, row by row and col by col, applying side effect
--
-- >>> matrix [[0,1],[2,3]] >>= iterateMatrix_  (\ r c e -> putStrLn $ show r ++ "," ++ show c ++ " = " ++ show e) 
-- 0,0 = 0.0
-- 0,1 = 1.0
-- 1,0 = 2.0
-- 1,1 = 3.0
iterateMatrix_ :: (Int -> Int -> Double -> IO a) -> Matrix -> IO ()
iterateMatrix_ f m = do
  let r = rows m
  let c = cols m
  let d = rawData m
  let ixs = [ (i,j) | i <- [0..r-1], j <- [0..c-1]]
  forM_ ixs (\ (i,j) -> readArray d (r * i + j) >>= f i j)

-- |Map a value over all cells in a matrix and update it.
--
-- This function mutates the given matrix
--
-- >>> matrix [[0,1],[2,3]] >>= updateMatrix  (\ r c e -> 2 * e)  >>= printMatrix >>= putStrLn
-- [0.0 , 2.0]
-- [4.0 , 6.0]  
updateMatrix :: (Int -> Int -> Double -> Double) -> Matrix -> IO Matrix
updateMatrix f m = do
  let r = rows m
  let c = cols m
  let d = rawData m
  let ixs = [ (i,j) | i <- [0..r-1], j <- [0..c-1]]
  forM_ ixs (\ (i,j) -> do
                let ix = (r * i + j)
                v <- readArray d ix
                writeArray d ix (f i j v))
  return m

-- |Accumulate a function over a matrix content
--
-- >>> matrix [[0,1],[2,3]] >>= foldMatrix (\ s _ _ e -> (s+1,e)) 0  >>= (putStrLn.show.fst)
-- 4
foldMatrix :: (s -> Int -> Int -> Double -> (s, Double)) -> s -> Matrix -> IO (s, Matrix)
foldMatrix f s m = do
  let r = rows m
  let c = cols m
  let d = rawData m
  let ixs = [ (i,j) | i <- [0..r-1], j <- [0..c-1]]
  s'<- foldM (\ s' (i,j) -> do
             let ix = (r * i + j)
             v <- readArray d ix
             let (s'',v') = (f s' i j v)
             writeArray d ix v'
             return s'') s ixs
  return (s', m)
  

-- |An empty matrix of given number of (rows,cols)
--
-- Content of matrix is initialized to 0
emptyMatrix :: (Int,Int) -> IO Matrix
emptyMatrix (r,c) = do
  a <- newArray (0,c*r) 0
  return $ Matrix a c r

-- |Make a mutable matrix from a list of double values
-- Assume matrix is well-formed, eg. all rows have equal number of arguments
matrix :: [[ Double ]] -> IO Matrix
matrix values = do
  let r = length values
  let c = length $ head values
  let ixs = [ r * i + j | i <- [0..r-1], j <- [0..c-1]]
  let elems = [ c | r <- values, c <- r]
  a <- newArray (0,c*r) 0
  forM (zip ixs elems) (uncurry $ writeArray a)
  return $ Matrix a r c

-- |Print a nicely formatted matrix
--
-- >>> matrix [[0,1],[2,3]] >>= printMatrix >>= putStrLn
-- [0.0 , 1.0]
-- [2.0 , 3.0]
printMatrix :: Matrix -> IO String
printMatrix m = mapM showRow [0 .. rows m-1] >>= (return.concat.intersperse "\n")
  where
    len = rows m
    a = rawData m
    
    showRow :: Int -> IO String
    showRow i = do
      r <- mapM showCell [ len * i + j | j <- [0 ..cols m-1]]
      return $ '[' : concat (intersperse " , " r)  ++ "]"
      
    showCell :: Int -> IO String
    showCell i = do
      c <- readArray a i
      return $ show c
      
