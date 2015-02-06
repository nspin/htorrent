module Curtis.Bencode
    ( BValue(..)
    , getString
    , getInt
    , getList
    , getDict
    , bookup
    , parseBen
    , bencode
    , hashify
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
            | BInt Integer
            | BList [BValue]
            | BDict [(C.ByteString, BValue)]
            deriving Show

-- Four getters for extracting info from bvalues

getString :: BValue -> Maybe C.ByteString
getString (BString v) = Just v
getString _ = Nothing

getInt :: BValue -> Maybe Integer
getInt (BInt v) = Just v
getInt _ = Nothing

getList :: BValue -> Maybe [BValue]
getList (BList v) = Just v
getList _ = Nothing

getDict :: BValue -> Maybe [(C.ByteString, BValue)]
getDict (BDict v) = Just v
getDict _ = Nothing

-- TODO improve (with attoparsec)
bookup :: String -> [(C.ByteString, a)] -> Maybe a
bookup skey = lookup (C.pack skey)

-- De-bencodes a bytestring
parseBen =  BString <$> parseString
        <|> BInt    <$> parseMid 'i' P.decimal -- make signed
        <|> BList   <$> parseMid 'l' (P.many1 parseBen)
        <|> BDict   <$> parseMid 'd' (P.many1 $ liftA2 (,) parseString parseBen)

parseMid start middle = P.char start *> middle <* P.char 'e'

parseString = P.decimal <* P.char ':' >>= P.take

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
hashify :: C.ByteString -> Maybe Word160
hashify = liftM (hash . B.unpack) . (
          ( P.maybeResult
          . P.parse ( parseMid 'd'
                    $ P.many1
                    $ liftA2 (,) parseString (fst <$> P.match parseBen)
                    )
          ) >=> bookup "info")
