module PCASpec where

import           Numeric.LinearAlgebra
import           PCA
import           Test.Hspec

spec :: Spec
spec = describe "PCA Computation" $ do

  it "compute PCA from known sample" $ do
    let raw = fromLists [ [2.5, 2.4]
                        , [0.5, 0.7]
                        , [2.2, 2.9]
                        , [1.9, 2.2]
                        , [3.1, 3.0]
                        , [2.3, 2.7]
                        , [2  , 1.6]
                        , [1  , 1.1]
                        , [1.5, 1.6]
                        , [1.1, 0.9]
                        ]

        (_,_,enc,dec) = pca 1 raw

    enc (toRows raw !! 0) `shouldBe` vector [ 0.8279701862010875 ]

  it "computes first component of fast PCA from known sample" $ do
    let raw = fromLists [ [2.5, 2.4]
                        , [0.5, 0.7]
                        , [2.2, 2.9]
                        , [1.9, 2.2]
                        , [3.1, 3.0]
                        , [2.3, 2.7]
                        , [2  , 1.6]
                        , [1  , 1.1]
                        , [1.5, 1.6]
                        , [1.1, 0.9]
                        ]

        [ v ] = fastPCA 1 raw

    (toRows raw !! 0 <.> v) `shouldBe` 0.8279701862010875
