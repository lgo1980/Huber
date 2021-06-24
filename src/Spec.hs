module Spec where
import PdePreludat
import Library
import Test.Hspec
 
-- tests
correrTests :: IO ()
correrTests = hspec $ do
  describe "Tests de Kata 4" $ do
   describe "Dieta imposible" $ do
     it "Debe detectar una dieta imposible" $ do
       2 + 2 `shouldBe` 4
