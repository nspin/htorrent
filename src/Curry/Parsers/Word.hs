module Curry.Parsers.Word
    ( Word128
    , Word160(..)
    , parse16
    , parse32
    , parse160
    , parse128
    , write16
    , write32
    , write128
    , write160
    , draw32
    ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Digest.SHA1 (Word160(..))
import           Data.List
import           Data.Word

----------------------------------------
-- WORDPLAY
----------------------------------------

type Word128 = (Word32, Word32, Word32, Word32)

parse16 :: Parser Word16
parse16 = mkWord16 <$> anyWord8 <*> anyWord8

parse32 :: Parser Word32
parse32 = mkWord32 <$> parse16 <*> parse16

parse128 :: Parser Word128
parse128 = (,,,) <$> parse32
                 <*> parse32
                 <*> parse32
                 <*> parse32

parse160 :: Parser Word160
parse160 = Word160 <$> parse32
                   <*> parse32
                   <*> parse32
                   <*> parse32
                   <*> parse32

--

write16 :: Word16 -> B.ByteString
write16 w = B.pack [a, b]
  where (a, b) = unMkWord16 w

write32 :: Word32 -> B.ByteString
write32 w = B.concat $ map write16 [a, b]
  where (a, b) = unMkWord32 w

write128 :: Word128 -> B.ByteString
write128 (a, b, c, d) = B.concat $ map write32 [a, b, c, d]

write160 :: Word160 -> B.ByteString
write160 (Word160 a b c d e) =  B.concat $ map write32 [a, b, c, d, e]

-- Show Word32 as an IPv4 address
draw32 :: Word32 -> String
draw32 = intercalate "." . map show . B.unpack . write32

--

mkWord16 :: Word8 -> Word8 -> Word16
mkWord16 a b = shift (fromIntegral a) 8 .|. fromIntegral b

mkWord32 :: Word16 -> Word16 -> Word32
mkWord32 a b =  shift (fromIntegral a) 16 .|. fromIntegral b

unMkWord16 :: Word16 -> (Word8, Word8)
unMkWord16 x = (fromIntegral $ shift x $ -8, fromIntegral x)

unMkWord32 :: Word32 -> (Word16, Word16)
unMkWord32 x = (fromIntegral $ shift x $ -16, fromIntegral x)
