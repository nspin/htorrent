module Curry.Parsers.Word
    ( Word128
    , Word160(..)
    , parse32
    , parse160
    , parse128
    , write32
    , write128
    , write160
    ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Digest.SHA1 (Word160(..))
import           Data.Word

----------------------------------------
-- WORDPLAY
----------------------------------------

type Word128 = (Word32, Word32, Word32, Word32)

parse32 :: Parser Word32
parse32 = mkWord32 <$> anyWord8
                   <*> anyWord8
                   <*> anyWord8
                   <*> anyWord8

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

write32 :: Word32 -> B.ByteString
write32 w = B.pack [a, b, c, d]
  where (a, b, c, d) = unMkWord32 w

write128 :: Word128 -> B.ByteString
write128 (a, b, c, d) = B.concat $ map write32 [a, b, c, d]

write160 :: Word160 -> B.ByteString
write160 (Word160 a b c d e) =  B.concat $ map write32 [a, b, c, d, e]

mkWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
mkWord32 a b c d =  shift (fromIntegral a) 24
                .|. shift (fromIntegral b) 16
                .|. shift (fromIntegral c) 8
                .|. fromIntegral d

unMkWord32 :: Word32 -> (Word8, Word8, Word8, Word8)
unMkWord32 x = ( fromIntegral $ shift x $ -24
               , fromIntegral $ shift x $ -16
               , fromIntegral $ shift x $ -8
               , fromIntegral x
               )
