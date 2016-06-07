module Main where

import           Criterion.Main
import           PCABench
import           System.Random

main :: IO ()
main = defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks =
  [
    bgroup "PCA"
    [
      bench "PCA 5"    $ nfIO $ computePCA 5
    , bench "PCA 10"   $ nfIO $ computePCA 10
    , bench "PCA 50"   $ nfIO $ computePCA 50
    , bench "PCA 100"  $ nfIO $ computePCA 100
    , bench "PCA 500"  $ nfIO $ computePCA 500
    , bench "PCA 1000" $ nfIO $ computePCA 1000
    , bench "PCA 2000" $ nfIO $ computePCA 2000
    , bench "PCA 3000" $ nfIO $ computePCA 3000
    ]
  ]

