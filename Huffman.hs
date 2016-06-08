{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Assign codes and inner layer to each word according to Huffman coding
module Huffman(Code,
               Bin(..),
               Coding(..),
               huffmanEncode,
               asNum) where
import           Data.HashMap.Strict (HashMap, empty, fromList, insert, toList)
import qualified Data.Heap           as H
import           Data.Maybe          (fromJust)

data Bin = Zero | One deriving (Eq, Ord, Show, Read)

type Code = [Bin]

asNum :: (Num a ) => Bin -> a
asNum Zero = 0
asNum One  = 1

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

data Coding = Coding {
  -- Index of word in corpus
  index      :: Int,

  -- Frequency of word in corpus
  frequency  :: Int,

   -- Huffman encoding of a word, LSB first
  huffman    :: Code,

  -- List of indices of path from root to word in encoding
  -- The indices are built in sucha way that most frequent words have smaller indices
  wordPoints :: [Int]
  } deriving (Eq, Show, Read)

huffmanEncode :: HashMap String Int -> HashMap String Coding
huffmanEncode = encode . arborify . heapify

instance Ord Coding where
  compare (Coding _ f _ _ ) (Coding _ f' _ _ ) = compare f f'

buildWord ::  ([Huffman],Int) -> (String, Int) -> ([Huffman],Int)
buildWord (ws, n) (w,f) = ((Leaf w (Coding n f [] [])):ws, n+1)

-- |Returns the list of words stored in given heap in ascending order.
ascWords :: H.MinHeap Huffman -> [ String ]
ascWords = map unWord . H.toAscList
  where
    unWord (Leaf w _) = w

-- |Build a heap from hashmap of words frequencies.
--
-- The heap is built with the frequency as ordering factor. Each word is built into a `Word`
-- object that contains the frequency, the index of the word relative to size of vocabulary.
--
-- >>> ascWords $ heapify (fromList [("foo",3), ("bar",2)])
-- ["bar","foo"]
heapify :: HashMap String Int -> H.MinHeap Huffman
heapify = foldl (flip H.insert) H.empty . fst . foldl buildWord ([],0) . toList

data Huffman =  Node Huffman Huffman Coding
               | Leaf String Coding
                 deriving (Eq,Show)

freq :: Huffman -> Int
freq (Node _ _ (Coding _ f _ _)) = f
freq (Leaf _ (Coding _ f _ _)) = f

instance Ord Huffman where
  compare (Node _ _ c)      (Node _ _ c')      = compare c c'
  compare (Node _ _ c)      (Leaf _ c') = compare c c'
  compare (Leaf _ c) (Node _ _ c')      = compare c c'
  compare (Leaf _ c) (Leaf _ c') = compare c c'


-- | Build a tree from heap with only words
--
arborify ::  H.MinHeap Huffman -> H.MinHeap Huffman
arborify h = foldl buildTree h [0.. sizeOfVocabulary -1]
  where
    sizeOfVocabulary = H.size h
    buildTree h n    = let (min1,h1) = fromJust $ H.view h
                       in case H.view h1 of
                         Just (min2,h2) -> H.insert (Node min1 min2
                                                     (Coding n
                                                      (freq min1 + freq min2)
                                                      []
                                                      [])) h2
                         Nothing        -> h


encode :: H.MinHeap Huffman -> HashMap String Coding
encode h = encode' (fst $ fromJust $ H.view h) [] [] empty
  where
    encode' (Leaf w c)          code points map = insert w c { huffman = code, wordPoints = points } map
    encode' (Node left right c) code points map = let pts = index c : points
                                                      m1  = encode' left (Zero:code) pts map
                                                  in
                                                   encode' right (One:code) pts m1

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

