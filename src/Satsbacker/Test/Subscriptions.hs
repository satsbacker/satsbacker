
module Satsbacker.Test.Subscriptions where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)





main :: Config -> IO ()
main Config{..} = hspec $ do
  describe "subscriptions" $ do
    it "" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
