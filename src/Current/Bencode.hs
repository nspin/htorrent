module Current.Bencode
    ( getBVal
    , getString
    , getInt
    , getList
    , getDict
    , hashify
    ) where

import           Curtis.Types

import           Control.Applicative
import           Control.Monad
import           Crypto.Hash.SHA1
import           Data.List
import           Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

----------------------------------------
-- PARSERS
----------------------------------------

-- Parses a bencoded value
parseBVal :: Parser BValue
parseBVal =  BString <$> parseString
         <|> BInt    <$> parseMid 'i' decimal -- make signed?
         <|> BList   <$> parseMid 'l' (many1 parseBVal)
         <|> BDict   <$> parseDict parseBVal

-- Parses a bencoded string
parseString :: Parser B.ByteString
parseString = decimal <* char ':' >>= P.take

-- Parse a list of (key,value)'s according to a parser for values
-- (generalized because used both in parseBVal and rawDict)
parseDict :: Parser a -> Parser [(String, a)]
parseDict = parseMid 'd' . many1 . liftA2 (,) (C.unpack <$> parseString)

-- Parses the between start and 'e'
parseMid :: Char -> Parser a -> Parser a
parseMid start middle = char start *> middle <* char 'e'

-- Extract raw bytestring of info key (if it exists), and calculate infohash
-- Note that >=>'s presidence is lower than that of >>=
hashify :: B.ByteString -> Maybe B.ByteString
hashify = fmap hash . (maybeResult . parse rawDict >=> lookup "info")

-- Parses a bencoded dictionary, leaving values as raw bytestrings
rawDict :: Parser [(String, B.ByteString)]
rawDict = parseDict $ fst <$> match parseBVal

----------------------------------------
-- PARSING-GETTER-THINGS (a -> Maybe b)
----------------------------------------

-- Type signature says all.

getBVal :: B.ByteString -> Maybe BValue
getBVal = maybeResult . parse parseBVal

-- Four getters for extracting info from bvals, one for each constructor
-- (the least interesting part of this module)

getString :: BValue -> Maybe B.ByteString
getString (BString v) = Just v
getString _ = Nothing

getInt :: BValue -> Maybe Int
getInt (BInt v) = Just v
getInt _ = Nothing

getList :: BValue -> Maybe [BValue]
getList (BList v) = Just v
getList _ = Nothing

getDict :: BValue -> Maybe [(String, BValue)]
getDict (BDict v) = Just v
getDict _ = Nothing