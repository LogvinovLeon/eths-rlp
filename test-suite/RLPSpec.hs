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
    it "empty string" $
      (RLP.rlpSerialize $ RLPItem B.empty) `shouldBe` (BL.pack [0x80])
    it "empty list" $
      (RLP.rlpSerialize $ RLPList []) `shouldBe` (BL.pack [0xc0])
