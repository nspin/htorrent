module Wing where

import Data.Word8
import Data.ByteString as B
import Data.ByteString.Char8 as C

type Wing = [Word8]

wap :: String -> Wing
wap = B.unpack . C.pack

-- I wish there were a better way...
wow :: Show a => a -> Wing
wow = wap . show

-- This is hacky and bad, but convenient.
escape :: Wing -> Wing
escape = wap . C.show . B.pack
