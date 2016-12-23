{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module RLPSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import RLP
import Data.Char
import Test.QuickCheck.Instances
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

default (B.ByteString)

data Empty = Empty
instance RLPSerializable Empty where
  toRLP _ = RLPList []

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
      serialize Empty `shouldBe` "\xc0"
    it "integer" $
      serialize "\x0f" `shouldBe` "\x0f"
    it "two byte integer" $
      serialize "\x04\x00" `shouldBe` "\x82\x04\x00"
    it "simple tree" $
      serialize [[],[[]],[[],[Empty]]] `shouldBe` "\xc7\xc0\xc1\xc0\xc3\xc0\xc1\xc0"
    it "lorem ipsum" $
      serialize "Lorem ipsum dolor sit amet, consectetur adipisicing elit" `shouldBe` "\xb8\x38Lorem ipsum dolor sit amet, consectetur adipisicing elit"
  describe "Properties" $ do
    it "Serialize is inverse to deserialize" $
      property $ (\x -> ((rlpDeserialize $ BL.toStrict $ serialize x) == (Right (toRLP x), "")))
