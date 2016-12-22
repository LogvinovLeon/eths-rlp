{-# LANGUAGE OverloadedStrings #-}
module RLPSpec (spec) where

import Test.Hspec
import RLP
import Data.Char
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

spec :: Spec
spec = do
  describe "RLP Examples" $ do
    it "simple string" $
      serialize "dog" `shouldBe` "\x83\&dog"
    it "simple list" $
      serialize ["cat", "dog"] `shouldBe` "\xc8\x83\&cat\x83\&dog"
    it "empty string" $
      serialize "" `shouldBe` "\x80"
    it "empty list" $
      serialize [] `shouldBe` "\xc0"
    it "integer" $
      serialize "\x0f" `shouldBe` "\x0f"
    it "two byte integer" $
      serialize "\x04\x00" `shouldBe` "\x82\x04\x00"
    it "simple tree" $
      serialize ([[],[[]],[[],[[]]]]::[[[[B.ByteString]]]]) `shouldBe` "0xc7\0xc0\0xc1\0xc0\0xc3\0xc0\0xc1\0xc0"
