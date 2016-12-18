module RLP where

import Data.Word
import qualified Data.ByteString as B

data RLPObject = RLPItem [Word8] | RLPList [RLPObject] deriving (Show, Eq, Ord);

class RLPSerializable a where
  rlpDecode::RLPObject -> a
  rlpEncode::a -> RLPObject

int2LE::Integral a => a -> [Word8]
int2LE i | i <= 0xff = [fromIntegral i]
int2LE i = least:rest
  where
    rest = int2Bytes $ i `div` 0xff
    least = fromIntegral $ i `mod` 0xff

int2BE::Integral a => a -> [Word8]
int2BE = reverse.int2LE

int2Bytes::Integral a => a -> [Word8]
int2Bytes = int2BE

-- | Serialize RLPObject using RLP encoding
--
-- Exmaples:
-- >>> rlp2Bytes $ RLPItem []
-- [128]
-- >>> rlp2Bytes $ RLPList []
-- [192]
rlp2Bytes::RLPObject -> [Word8]
rlp2Bytes (RLPItem [b]) | b <= 0x7f = [b]
rlp2Bytes (RLPItem bs) | length bs <= 55 = 0x80 + (fromIntegral $ length bs):bs
rlp2Bytes (RLPItem bs) = 0xb7 + (fromIntegral $ length $ lenb):lenb ++ bs
  where
    lenb = int2Bytes $ length bs
rlp2Bytes (RLPList os) | len <= 55 = 0xc0 + (fromIntegral $ len):internal
  where
    len = length internal
    internal = concat $ rlp2Bytes <$> os
rlp2Bytes (RLPList os) =  0xf7 + (fromIntegral $ length $ lenb):lenb ++ internal
  where
    lenb = int2Bytes $ length internal
    internal = concat $ rlp2Bytes <$> os

-- | Serialize RLPObject to ByteString
--
-- Examples:
-- >>> rlpSerialize $ RLPItem []
-- "\128"
-- >>> rlpSerialize $ RLPList []
-- "\192"
rlpSerialize::RLPObject -> B.ByteString
rlpSerialize o = B.pack $ rlp2Bytes o

