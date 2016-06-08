module Main where

import           Criterion.Main
import           PCABench
import           System.Random

main :: IO ()
main = defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks =
  [bgroup "PCA Fast for top 1 component"
     [ bench "PCA 1/5"    $ nfIO $ computeFastPCA 1 5
     , bench "PCA 1/10"   $ nfIO $ computeFastPCA 1 10
     , bench "PCA 1/50"   $ nfIO $ computeFastPCA 1 50
     , bench "PCA 1/100"  $ nfIO $ computeFastPCA 1 100
     , bench "PCA 1/500"  $ nfIO $ computeFastPCA 1 500
     , bench "PCA 1/1000"  $ nfIO $ computeFastPCA 1 1000
     ]
  ,  bgroup "PCA Fast for top 2 components"
     [ bench "PCA 2/5"    $ nfIO $ computeFastPCA 2 5
     , bench "PCA 2/10"   $ nfIO $ computeFastPCA 2 10
     , bench "PCA 2/50"   $ nfIO $ computeFastPCA 2 50
     , bench "PCA 2/100"  $ nfIO $ computeFastPCA 2 100
     , bench "PCA 2/500"  $ nfIO $ computeFastPCA 2 500
     , bench "PCA 2/1000"  $ nfIO $ computeFastPCA 2 1000
     ]
  ,  bgroup "PCA SVD for top 1 component"
     [ bench "PCA 1/5"    $ nfIO $ computePCASVD 1 5
     , bench "PCA 1/10"   $ nfIO $ computePCASVD 1 10
     , bench "PCA 1/50"   $ nfIO $ computePCASVD 1 50
     , bench "PCA 1/100"  $ nfIO $ computePCASVD 1 100
     , bench "PCA 1/500"  $ nfIO $ computePCASVD 1 500
     , bench "PCA 1/1000"  $ nfIO $ computePCASVD 1 1000
     ]
  ,  bgroup "PCA SVD for top 2 components"
     [ bench "PCA 2/5"    $ nfIO $ computePCASVD 2 5
     , bench "PCA 2/10"   $ nfIO $ computePCASVD 2 10
     , bench "PCA 2/50"   $ nfIO $ computePCASVD 2 50
     , bench "PCA 2/100"  $ nfIO $ computePCASVD 2 100
     , bench "PCA 2/500"  $ nfIO $ computePCASVD 2 500
     , bench "PCA 2/1000"  $ nfIO $ computePCASVD 2 1000
     ]
  ]

