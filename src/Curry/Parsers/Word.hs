module Curry.Parsers.Word
    ( parse32
    , parse160
    , write32
    , write160
    , Word160(..)
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

parse32 :: Parser Word32
parse32 = mkWord32 <$> anyWord8
                   <*> anyWord8
                   <*> anyWord8
                   <*> anyWord8

parse160 :: Parser Word160
parse160 = Word160 <$> parse32
                   <*> parse32
                   <*> parse32
                   <*> parse32
                   <*> parse32

write32 :: Word32 -> B.ByteString
write32 w = B.pack [a, b, c, d]
  where (a, b, c, d) = unMkWord32 w

write160 :: Word160 -> B.ByteString
write160 (Word160 a b c d e) =  B.concat $ map write32 [a, b, c, d, e]

mkWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
mkWord32 a b c d =  shiftL (fromIntegral a) 24
                .|. shiftL (fromIntegral b) 16
                .|. shiftL (fromIntegral c) 8
                .|. shiftL (fromIntegral d) 0

unMkWord32 :: Word32 -> (Word8, Word8, Word8, Word8)
unMkWord32 w = (part 24, part 16, part 8, part 0)
  where part = fromIntegral . shiftR w

curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 f w x y z = f (w, x, y, z)

unCurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
unCurry4 f (w, x, y, z) = f w x y z
