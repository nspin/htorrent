module Bencode ( BValue(..)
               , readBen
               , writeBen
               , rawInfo
               ) where

import           Control.Monad
import           Control.Applicative

import           Data.List
import           Data.Digest.SHA1

import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString.Char8 as P

-- A bencoded value
data BValue = BString B.ByteString
            | BInt Int
            | BList [BValue]
            | BDict [(Wing, BValue)]
            deriving Show

-- Bencodes a BValue into a wing
writeBen :: BValue -> Wing
writeBen (BString bytes) = writeBytes bytes
writeBen (BInt    int  ) = surround 'i' $ show int
writeBen (BList   list ) = surround 'l' $ concatMap writeBen list
writeBen (BDict   dict ) = surround 'd' $ concatMap writePair dict

surround :: Word8 -> Wing -> Wing
surround start middle = start : middle ++ [_e]

writePair :: (Wing, BValue) -> Wing
writePair (key, value) = writeBytes key ++ writeBen value

writeBytes :: Wing -> Wing
writeBytes bytes = wow (length bytes) ++ _colon : bytes

-- De-bencodes a bytestring
readBen :: B.ByteString -> Maybe BValue
readBen = P.maybeResult . P.parse parseValue

parseValue =  BString <$> parseString
          <|> BInt    <$> parseMid 'i' P.decimal -- make signed
          <|> BList   <$> parseMid 'l' (P.many1 parseValue)
          <|> BDict   <$> parseMid 'd' (P.many1 $ liftA2 (,) parseString parseValue)

parseMid start middle = P.char start *> middle <* P.char 'e'

parseString = do
    len <- P.decimal <* P.char ':'
    P.count len P.anyChar

-- Extract raw bytestring of info key (if it exists), for use in calculating infohash
rawInfo :: B.ByteString -> Maybe String
rawInfo = ( P.maybeResult
          . P.parse ( parseMid 'd'
                    $ P.many1
                    $ liftA2 (,) parseString ((B.unpack . fst) <$> P.match parseValue)
                    )
          ) >=> lookup "info"
