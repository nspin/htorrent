module Curtis.Parsers.Bencode
    ( BValue(..)
    , getBen
    , getString
    , getInt
    , getList
    , getDict
    , bookup
    , bencode
    , hashify
    ) where

import           Control.Applicative
import           Control.Monad
import           Crypto.Hash.SHA1
import           Data.List
import           Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

getBen :: B.ByteString -> Maybe BValue
getBen = maybeResult . parse parseBen

-- Four getters for extracting info from bvalues

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

-- TODO improve (with attoparsec)
bookup :: String -> [(C.ByteString, a)] -> Maybe a
bookup skey = lookup (C.pack skey)

-- De-bencodes a bytestring
parseBen =  BString <$> parseString
        <|> BInt    <$> parseMid 'i' decimal -- make signed
        <|> BList   <$> parseMid 'l' (many1 parseBen)
        <|> BDict   <$> parseMid 'd' (many1 $ liftA2 (,) parseString parseBen)

parseMid start middle = char start *> middle <* char 'e'

parseString = decimal <* char ':' >>= P.take

-- Bencodes a BValue into a wing
bencode :: BValue -> C.ByteString
bencode (BString bytes) = writeBytes bytes
bencode (BInt    int  ) = surround 'i' . C.pack $ show int
bencode (BList   list ) = surround 'l' . C.concat $ map bencode list
bencode (BDict   dict ) = surround 'd' . C.concat $ map writePair dict

surround :: Char -> C.ByteString -> C.ByteString
surround start = (start `C.cons`) . (`C.snoc` 'e')

writePair :: (C.ByteString, BValue) -> C.ByteString
writePair (key, value) = writeBytes key `C.append` bencode value

writeBytes :: C.ByteString -> C.ByteString
writeBytes bytes = (C.pack . show $ C.length bytes) `C.append` C.cons ':' bytes

-- Extract raw bytestring of info key (if it exists), for use in calculating infohash
hashify :: B.ByteString -> Maybe B.ByteString
hashify = liftM hash . (
          marse ( parseMid 'd'
                $ many1
                $ liftA2 (,) parseString (fst <$> match parseBen)
                )
          >=> bookup "info")
