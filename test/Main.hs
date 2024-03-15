module Main (main) where

import Data.Json
import Data.Json.Types
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "json parsing" $ do
    it "should parse number" $ do
      runParser jsonValue "123" `shouldBe` Just ("", JsonNumber 123)

    it "should not parse empty number" $ do
      runParser jsonNumber "" `shouldBe` Nothing

    it "should parse object" $ do
      runParser jsonValue "{\"yay\": 1}"
        `shouldBe` Just ("", JsonObject [("yay", JsonNumber 1)])
