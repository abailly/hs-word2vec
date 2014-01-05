{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Assign codes and inner layer to each word according to Huffman coding
module Huffman where
import Data.HashMap.Strict(empty,
                         insertWith,
                         HashMap,
                         toList,
                         fromList)
import qualified Data.Heap as H

data Bin = Zero | One deriving (Eq, Ord, Show, Read)

type Code = [Bin]

instance Enum Code where
  toEnum n = case n `mod` 2 of
    0 -> Zero : toEnum' (n `div` 2)
    1 -> One : toEnum' (n `div` 2) 
    where
      toEnum' 0 = []
      toEnum' n = toEnum n
      
  fromEnum []       = 0
  fromEnum (Zero:b) = 2 * fromEnum b
  fromEnum (One:b)  = 2 * fromEnum b + 1
  
data Huffman a =  Node (Huffman a) (Huffman a)
               | Leaf a

data Coding = Coding {
  -- Frequency of word in corpus
  frequency  :: Int,
  
   -- Huffman encoding of a word, LSB first
  huffman    :: Code,

  -- List of indices of path from root to word in encoding
  -- The indices are built in sucha way that most frequent words have smaller indices
  wordPoints :: [Int]
  } deriving (Eq, Show, Read)
             
code :: HashMap String Int -> HashMap String Coding
code _ = empty

newtype WordFreq = WF (String, Int) deriving (Eq, Show)

instance Ord WordFreq where
  compare (WF (_,x)) (WF (_,y)) = compare x y
  
-- | Build a heap from hashmap of words
--
-- >>> heapify (fromList [("foo",3), ("bar",2)])
-- fromList [(WF ("bar",2),()),(WF ("foo",3),())]
heapify :: HashMap String Int -> H.MinHeap WordFreq
heapify = foldl (flip H.insert) H.empty . map WF . toList

-- # Tests
    
-- | test Enum implementation
--
-- >>> toEnum 0 :: Code
-- [Zero]
-- >>> toEnum 1 :: Code
-- [One]
-- >>> toEnum 2 :: Code
-- [Zero,One]
testToEnum :: Int -> Code
testToEnum = toEnum

-- | test Enum implementation
--
-- >>> fromEnum [Zero]
-- 0
-- >>> fromEnum [One]
-- 1
-- >>> fromEnum [Zero,One]
-- 2
-- >>> fromEnum [Zero,One,Zero,One]
-- 10
testFromEnum :: Code -> Int
testFromEnum = fromEnum

