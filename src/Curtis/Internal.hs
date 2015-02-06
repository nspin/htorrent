module Curtis.Internal
    ( encode8
    , encode160
    , read160
    ) where

import           Data.Bits
import           Data.Word
import           Data.List
import           Data.List.Split
import           Data.Digest.SHA1
import qualified Data.ByteString as B

encode160 :: Word160 -> String
encode160 (Word160 a b c d e) = [a, b, c, d, e] >>= chunk32 >>= (encode8 . fromIntegral)

encode8 :: Word8 -> String
encode8 byte = ['%', toHexHalf $ shiftR byte 4, toHexHalf $ byte .&. 15]

toHexHalf :: Word8 -> Char
toHexHalf = genericIndex "0123456789ABCDEF"

chunk32 :: Word32 -> [Word32]
chunk32 x = [ shiftR x 24
            , shiftR x 16
            , shiftR x  8
            , x
            ]

unchunk32 :: [Word32] -> Word32
unchunk32 [a, b, c, d] =  shiftL a 24
                      .&. shiftL b 16
                      .&. shiftL c  8
                      .&. d

read160 :: B.ByteString -> Maybe Word160
read160 bytes = case parts of
    [a, b, c, d, e] -> Just $ Word160 a b c d e
    _ -> Nothing
  where parts = map unchunk32 . chunksOf 4 . map fromIntegral $ B.unpack bytes
