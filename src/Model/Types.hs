{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Interface and core types describing a training model
module Model.Types where

import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.Vector         as V
import           GHC.Generics
import           Huffman

-- | A "generic" type of vectors
newtype WordVector = W { vector :: V.Vector Double }
                     deriving (Show,Generic)

instance ToJSON WordVector

class NN a where
  type Vector a :: *
  type Layer a :: *

  vectorize :: Vector a -> WordVector

  initialize :: (MonadIO m)
                => Int        -- number of words
                -> Int        -- number of features (dimensions)
                -> m a

  updateSinglePoint :: (MonadIO m)
                       => Vector a
                       -> Double
                       -> (Vector a, Layer a)
                       -> (Int, Bin)
                       -> m (Vector a, Layer a)

  similarity :: a -> String -> String -> IO Double
