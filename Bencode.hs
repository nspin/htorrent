module Bencode ( BValue(..)
               , readBen
               , writeBen
               , getDict
               , getList
               , getString
               , getInt
               , lookup
               -- , rawInfo
               ) where

import           Control.Monad
import           Control.Applicative
import           Data.List
import           Data.Digest.SHA1

import           Data.Word
import           Data.Word8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as P

type Wing = [Word8]

wap :: String -> Wing
wap = B.unpack . C.pack

-- I wish there were a better way...
wow :: Show a => a -> Wing
wow = wap . show

-- A bencoded value
data BValue = BString Wing
            | BInt Word
            | BList [BValue]
            | BDict [(Wing, BValue)]
            deriving Show

-- Getters for parsing de-bencoded torrent files

getString :: BValue -> Maybe Wing
getString (BString v) = Just v
getString _ = Nothing

getInt :: BValue -> Maybe Word
getInt (BInt v) = Just v
getInt _ = Nothing

getList :: BValue -> Maybe [BValue]
getList (BList v) = Just v
getList _ = Nothing

getDict :: BValue -> Maybe [(Wing, BValue)]
getDict (BDict v) = Just v
getDict _ = Nothing

-- Bencodes a bytestring
writeBen :: BValue -> Wing
writeBen (BString bytes) = writeBytes bytes
writeBen (BInt    n    ) = surround _i $ wow n
writeBen (BList   list ) = surround _l $ concatMap writeBen list
writeBen (BDict   dict ) = surround _d $ concatMap writePair dict

writePair :: (Wing, BValue) -> Wing
writePair (key, value) = writeBytes key ++ writeBen value

writeBytes :: Wing -> Wing
writeBytes bytes = wow (length bytes) ++ _colon : bytes

surround :: Word8 -> Wing -> Wing
surround start middle = start : middle ++ [_e]

-- De-bencodes a bytestring
readBen :: B.ByteString -> Maybe BValue
readBen = P.maybeResult . P.parse parseValue

parseValue =  BString <$> parseWing
          <|> BInt    <$> parseMid 'i' P.decimal
          <|> BList   <$> parseMid 'l' (P.many1 parseValue)
          <|> BDict   <$> parseMid 'd' (P.many1 $ liftA2 (,) parseWing parseValue)

parseMid start middle = P.char start *> middle <* P.char 'e'

parseWing = do
    len <- P.decimal <* P.char ':'
    A.count len A.anyWord8

-- Extract raw bytestring of info key (if it exists), for use in calculating infohash
-- rawInfo :: B.ByteString -> Maybe Wing
-- rawInfo = fmap B.unpack
--         . ( P.maybeResult
--           . P.parse ( parseMid 'd'
--                     $ P.many1
--                     $ liftA2 (,) parseWing (fst <$> P.match parseValue)
--                     )
--           ) >=> lookup (wap "info")
