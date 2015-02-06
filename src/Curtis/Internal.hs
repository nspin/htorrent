module Curtis.Internal
    ( marse
    , urify160
    , parse160
    ) where

import           Data.Bits
import           Data.Word
import           Data.List
import           Data.Digest.SHA1
import qualified Data.ByteString as B
import           Data.Attoparsec.ByteString

marse :: Parser a -> B.ByteString -> Maybe a
marse parser = maybeResult . parse parser

urify160 :: Word160 -> String
urify160 (Word160 a b c d e) = [a, b, c, d, e] >>= chunk32 >>= (urify8 . fromIntegral)

urify8 :: Word8 -> String
urify8 byte = ['%', toHexHalf $ shiftR byte 4, toHexHalf $ byte .&. 15]

toHexHalf :: Word8 -> Char
toHexHalf = genericIndex "0123456789ABCDEF"

chunk32 :: Word32 -> [Word32]
chunk32 x = [ shiftR x 24
            , shiftR x 16
            , shiftR x  8
            , x
            ]

parse160 :: Parser Word160
parse160 = do
    a <- parse32
    b <- parse32
    c <- parse32
    d <- parse32
    e <- parse32
    return $ Word160 a b c d e

parse32 :: Parser Word32
parse32 = do
    a <- anyWord8
    b <- anyWord8
    c <- anyWord8
    d <- anyWord8
    return $  shiftL (fromIntegral a) 24
          .&. shiftL (fromIntegral b) 16
          .&. shiftL (fromIntegral c)  8
          .&.        (fromIntegral d)

-- TODO: THIS

-- Not using attoparsec's hex parser because that does not take length into account.

-- parse128b16 :: C.Parser (Word64, Word64)
-- parse128b16 

-- parseHexByte :: C.Parser -> Word8
-- parseHexByte bytes = do
