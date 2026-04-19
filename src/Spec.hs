module Spec where
import PdePreludat
import Library
import Test.Hspec

individualDePalmitos = Pizza ["salsa", "muzzarella", "palmitos"] 4 300

correrTests :: IO ()
correrTests = hspec $ do
  describe "Pizza Conmigo" $ do
    it "La de palmitos es la única horrenda" $ do
      nivelDeSatisfaccion individualDePalmitos `shouldBe` 0