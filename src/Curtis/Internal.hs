module Internal
    ( encode8
    , encode160
    ) where

encode160 :: Word160 -> String
encode160 (Word160 a b c d e) = [a, b, c, d, e] >>= chunk32 >>= encode8

encode8 :: Word8 -> String
encode8 byte = ['%', toHexHalf $ shiftR byte 4, toHexHalf $ byte .&. 15]

toHexHalf :: Word8 -> Char
toHexHalf = genericIndex "0123456789ABCDEF"

chunk32 :: Word32 -> [Word8]
chunk32 x = map fromIntegral [ shiftR x 24
                             , shiftR x 16
                             , shiftR x  8
                             , x
                             ]
