module Bencode ( BValue(..)
               , readBen
               , writeBen
               , getDict
               , getList
               , getString
               , getInt
               , lookP
               , rawInfo
               ) where

import           Control.Monad
import           Control.Applicative
import           Data.List
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.Digest.SHA1
import qualified Data.ByteString.Char8 as B

-- A bencoded value
data BValue = BString B.ByteString
            | BInt Integer
            | BList [BValue]
            | BDict [(B.ByteString, BValue)]
            deriving Show

-- Getters for parsing de-bencoded torrent files

getString :: BValue -> Maybe B.ByteString
getString (BString v) = Just v
getString _ = Nothing

getInt :: BValue -> Maybe Integer
getInt (BInt v) = Just v
getInt _ = Nothing

getList :: BValue -> Maybe [BValue]
getList (BList v) = Just v
getList _ = Nothing

getDict :: BValue -> Maybe [(B.ByteString, BValue)]
getDict (BDict v) = Just v
getDict _ = Nothing
             
-- Bencodes a bytestring
writeBen :: BValue -> B.ByteString
writeBen (BString bytes) = writeBytes bytes
writeBen (BInt    n    ) = surround 'i' . B.pack $ show n
writeBen (BList   list ) = surround 'l' . B.concat $ map writeBen list
writeBen (BDict   dict ) = surround 'd' . B.concat $ map writePair dict

writePair :: (B.ByteString, BValue) -> B.ByteString
writePair (key, value) = B.append (writeBytes key) (writeBen value)

writeBytes :: B.ByteString -> B.ByteString
writeBytes bytes = B.append (B.pack . show $ B.length bytes) $ B.cons ':' bytes

surround :: Char -> B.ByteString -> B.ByteString
surround start = B.cons start . (`B.snoc` 'e')

-- De-bencodes a bytestring
readBen :: B.ByteString -> Maybe BValue
readBen = P.maybeResult . P.parse parseValue

parseValue =  BString <$> parseString
          <|> BInt    <$> parseMid 'i' P.decimal
          <|> BList   <$> parseMid 'l' (P.many1 parseValue)
          <|> BDict   <$> parseMid 'd' (P.many1 $ liftA2 (,) parseString parseValue)

parseMid start middle = P.char start *> middle <* P.char 'e'

parseString = P.decimal <* P.char ':' >>= P.take

lookP :: String -> [(B.ByteString, a)] -> Maybe a
lookP stringKey dict = lookup (B.pack stringKey) dict

-- Extract raw bytestring of info key (if it exists), for use in calculating infohash
rawInfo :: B.ByteString -> Maybe B.ByteString
rawInfo = ( P.maybeResult . P.parse ( parseMid 'd'
                                    $ P.many1
                                    $ liftA2 (,) parseString (fst <$> P.match parseValue)
                                    )
          ) >=> lookP "info"
