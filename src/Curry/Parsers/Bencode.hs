{-# LANGUAGE DeriveDataTypeable #-}

module Curry.Parsers.Bencode
    ( BValue(..)
    , getBVal
    , getString
    , getInt
    , getList
    , getDict
    , hashify
    , leekup
    ) where

import           Control.Applicative
import           Control.Monad
import           Crypto.Hash.SHA1
import           Data.List
import           Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Data
import           Data.Typeable

-- A bencoded value.
-- Note that bdict keys are strings, not bytestrings (or, if you prefer,
-- that bstrings are bytestrings). These are different because bstrings
-- may be binary data, whereas (in all implementations I know of) keys
-- are always text. Storing keys as strings allows string literals to
-- be used in lookup (rather than packing for each lookup, which is not
-- very efficient).
data BValue = BString B.ByteString
            | BInt Integer
            | BList [BValue]
            | BDict [(String, BValue)]
            deriving (Eq, Show, Data, Typeable)

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
hashify :: B.ByteString -> Either String B.ByteString
hashify = fmap hash . (eitherResult . parse rawDict >=> leekup "info")

-- Parses a bencoded dictionary, leaving values as raw bytestrings
rawDict :: Parser [(String, B.ByteString)]
rawDict = parseDict $ fst <$> match parseBVal

----------------------------------------
-- PARSING-GETTER-THINGS (a -> Either String b)
----------------------------------------

-- Type signature says all.

getBVal :: B.ByteString -> Either String BValue
getBVal = eitherResult . parse parseBVal

-- Four getters for extracting info from bvals, one for each constructor
-- (the least interesting part of this module)

getString :: BValue -> Either String B.ByteString
getString (BString v) = Right v
getString b = Left ("The following BValue is not a BString:\n" ++ show b)

getInt :: BValue -> Either String Integer
getInt (BInt v) = Right v
getInt b = Left ("The following BValue is not an BInt:\n" ++ show b)

getList :: BValue -> Either String [BValue]
getList (BList v) = Right v
getList b = Left ("The following BValue is not a BList:\n" ++ show b)

getDict :: BValue -> Either String [(String, BValue)]
getDict (BDict v) = Right v
getDict b = Left ("The following BValue is not a BDict:\n" ++ show b)

----------------------------------------
-- MISC
----------------------------------------

leekup :: Show a => String -> [(String, a)] -> Either String a
leekup key dict = case lookup key dict of
    Just val -> Right val
    Nothing -> Left ("Could not find key \"" ++ key ++ "\" in the following BDict:\n" ++ show dict)
