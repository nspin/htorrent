module Bencode ( BValue(..)
               , readBen
               , writeBen
               , hashify
               , bookup
               ) where

import           Control.Monad
import           Control.Applicative

import           Data.List
import           Data.Digest.SHA1

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Attoparsec.ByteString.Char8 as P

-- A bencoded value
data BValue = BString C.ByteString
            | BInt Int
            | BList [BValue]
            | BDict [(C.ByteString, BValue)]
            deriving Show

-- Bencodes a BValue into a wing
writeBen :: BValue -> C.ByteString
writeBen (BString bytes) = writeBytes bytes
writeBen (BInt    int  ) = surround 'i' . C.pack $ show int
writeBen (BList   list ) = surround 'l' . C.concat $ map writeBen list
writeBen (BDict   dict ) = surround 'd' . C.concat $ map writePair dict

surround :: Char -> C.ByteString -> C.ByteString
surround start = (start `C.cons`) . (`C.snoc` 'e')

writePair :: (C.ByteString, BValue) -> C.ByteString
writePair (key, value) = writeBytes key `C.append` writeBen value

writeBytes :: C.ByteString -> C.ByteString
writeBytes bytes = (C.pack . show $ C.length bytes) `C.append` C.cons ':' bytes

-- De-bencodes a bytestring
readBen :: C.ByteString -> Maybe BValue
readBen = P.maybeResult . P.parse parseValue

parseValue =  BString <$> parseString
          <|> BInt    <$> parseMid 'i' P.decimal -- make signed
          <|> BList   <$> parseMid 'l' (P.many1 parseValue)
          <|> BDict   <$> parseMid 'd' (P.many1 $ liftA2 (,) parseString parseValue)

parseMid start middle = P.char start *> middle <* P.char 'e'

parseString = P.decimal <* P.char ':' >>= P.take

-- Extract raw bytestring of info key (if it exists), for use in calculating infohash
hashify :: C.ByteString -> Maybe Word160
hashify = liftM (hash . B.unpack) . (
          ( P.maybeResult
          . P.parse ( parseMid 'd'
                    $ P.many1
                    $ liftA2 (,) parseString (fst <$> P.match parseValue)
                    )
          ) >=> bookup "info")

-- TODO improve (with attoparsec)
bookup :: String -> [(C.ByteString, a)] -> Maybe a
bookup skey = lookup (C.pack skey)
    
    
