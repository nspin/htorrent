{-# LANGUAGE TemplateHaskell #-}

module Curry.Parsers.Bencode
    ( BValue(..)
    , BDict
    , _BString
    , _BInt
    , _BList
    , _BDict
    , parseBValue
    , parseBDict
    , parseHash
    , writeBValue
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Digest.SHA1
import           Data.List hiding (take)
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (char, decimal, signed)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Prelude hiding (take)

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
            | BDict BDict
            deriving Show

type BDict = [(String, BValue)]

----------------------------------------
-- PARSERS
----------------------------------------

-- Parses a bencoded value
parseBValue :: Parser BValue
parseBValue =  BString <$> parseString
           <|> BInt    <$> parseMid 'i' (signed decimal)
           <|> BList   <$> parseMid 'l' (many1 parseBValue)
           <|> BDict   <$> parseBDict

parseBDict :: Parser BDict
parseBDict = parseADict parseBValue

-- Parses a bencoded string
parseString :: Parser B.ByteString
parseString = decimal <* char ':' >>= take

-- Parse a list of (key,value)'s according to a parser for values
-- (generalized because used both in parseBVal and rawDict)
parseADict :: Parser a -> Parser [(String, a)]
parseADict = parseMid 'd' . many1 . liftA2 (,) (C.unpack <$> parseString)

-- Parses the between start and 'e'
parseMid :: Char -> Parser a -> Parser a
parseMid start middle = char start *> middle <* char 'e'

-- Extract raw bytestring of info key (if it exists), and calculate infohash
parseHash :: Parser Word160
parseHash = do
    dict <- parseADict (match parseBValue)
    case lookup "info" dict of
        Nothing -> empty
        Just (bytes, _) -> return . hash $ B.unpack bytes

----------------------------------------
-- WRITERS
----------------------------------------

writeBValue :: BValue -> B.ByteString
writeBValue (BString bytes) = C.pack (show (B.length bytes) ++ ":") `B.append` bytes
writeBValue (BInt n) = surround 'i' . C.pack $ show n
writeBValue (BList bvals) = surround 'l' . B.concat $ map writeBValue bvals
writeBValue (BDict assocs) = surround 'd' $ B.concat
    [ C.pack (show (length key) ++ ':' : key) `B.append` writeBValue bval
    | (key, bval) <- assocs
    ]

surround :: Char -> B.ByteString -> B.ByteString
surround = (.) (`C.snoc` 'e') . C.cons

----------------------------------------
-- TEMPLATE HASKELL
----------------------------------------

makePrisms ''BValue
