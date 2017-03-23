{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Blockchain.Ethereum.RLP (rlpSerialize, rlpDeserialize, RLPSerializable (..), RLPObject (..)) where

import Data.Word
import Control.Applicative
import Control.Monad
import Data.Binary.Strict.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
data RLPObject = RLPItem B.ByteString | RLPList [RLPObject] deriving (Show, Eq, Ord);

int2LE::Integral a => a -> [Word8]
int2LE i | i <= 0xff = [fromIntegral i]
int2LE i = least:rest
  where
    rest = int2LE $ i `div` 0xff
    least = fromIntegral $ i `mod` 0xff

int2BE::Integral a => a -> [Word8]
int2BE = reverse.int2LE

int2Bytes::Integral a => a -> B.ByteString
int2Bytes = B.pack.int2BE

be2Int::Integral a => [Word8] -> a
be2Int = foldl (\v a -> v * 0xff + fromIntegral a) 0

bytes2Int::B.ByteString -> Int
bytes2Int = be2Int.B.unpack

rlp2Bytes::RLPObject -> Put
rlp2Bytes (RLPItem bs) | B.length bs == 1 && B.head bs <= 0x7f = putByteString bs
rlp2Bytes (RLPItem bs) | B.length bs <= 55 = do
  putWord8 $ 0x80 + fromIntegral (B.length bs)
  putByteString bs
rlp2Bytes (RLPItem bs) = do
  putWord8 $ 0xb7 + fromIntegral (B.length lenb)
  putByteString lenb
  putByteString bs
  where
    lenb = int2Bytes $ B.length bs
rlp2Bytes (RLPList os) | len <= 55 = do
  putWord8 $ 0xc0 + fromIntegral len
  putLazyByteString internal
  where
    len = LB.length internal
    internal = LB.concat $ runPut `fmap` rlp2Bytes `fmap` os
rlp2Bytes (RLPList os) = do
  putWord8 $ 0xf7 + fromIntegral (B.length lenb)
  putByteString lenb
  putLazyByteString internal
  where
    lenb = int2Bytes $ LB.length internal
    internal = LB.concat (runPut . rlp2Bytes <$> os)

-- | Serialize RLPObject to ByteString
--
-- Examples:
--
-- >>> rlpSerialize $ RLPItem B.empty
-- "\128"
-- >>> rlpSerialize $ RLPList []
-- "\192"
rlpSerialize::RLPObject -> LB.ByteString
rlpSerialize o = runPut $ rlp2Bytes o

bytes2RLP::Get RLPObject
bytes2RLP = do
  b <- getWord8
  case b of
    _ | b <= 0x7f -> return $ RLPItem $ B.singleton b
    _ | b <= 0xb7 -> do
      bs <- getByteString $ fromIntegral b - 0x80
      return $ RLPItem bs
    _ | b <= 0xbf -> do
      lengthBytes <- getByteString $ fromIntegral b - 0xb7
      let dataLength = bytes2Int lengthBytes
      bs <- getByteString dataLength
      return $ RLPItem bs
    _ | b <= 0xf7 -> do
      let listLength = fromIntegral b - 0xc0
      os <- replicateM listLength bytes2RLP
      return $ RLPList os
    _            -> do
      lengthBytes <- getByteString $ fromIntegral b - 0xf7
      let listLength = bytes2Int lengthBytes
      os <- replicateM listLength bytes2RLP
      return $ RLPList os

-- | Deserialize ByteString to RLPObject
--
-- Examples:
--
-- >>> rlpDeserialize $ B.pack [128]
-- (Right (RLPItem ""),"")
-- >>> rlpDeserialize $ B.pack [192]
-- (Right (RLPList []),"")
rlpDeserialize::B.ByteString -> (Either String RLPObject, B.ByteString)
rlpDeserialize = runGet bytes2RLP

class RLPSerializable a where
  toRLP::a->RLPObject
  serialize::a->LB.ByteString
  serialize = rlpSerialize.toRLP

instance RLPSerializable a => RLPSerializable [a] where
  toRLP os = RLPList $ toRLP `fmap` os

instance RLPSerializable B.ByteString where
  toRLP = RLPItem
