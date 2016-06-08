module PCASpec where

import           Numeric.LinearAlgebra
import           PCA
import           Test.Hspec

spec :: Spec
spec = describe "PCA Computation" $ do
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

  it "compute first component of PCA using SVD from known sample" $ do
    let (m,_,enc,dec) = pca 1 raw

    (toRows raw !! 0 <.> toRows m !! 0) `shouldBe` 3.459112269626609

  it "computes first component of PCA with NIPALS from known sample" $ do
    let [ v ] = pcaNipals 1 raw

    (toRows raw !! 0 <.> v) `shouldBe` 3.461356337247943

  it "computes first component of PCA with power iteration from known sample" $ do
    let [ v ] = fastPCA' 1 raw

    (toRows raw !! 0 <.> v) `shouldBe` 3.461356337251494

  it "computes first component of fast PCA from known sample" $ do
    let [ v ] = fastPCA 1 raw

    (toRows raw !! 0 <.> v) `shouldBe` 3.4591122696276297
