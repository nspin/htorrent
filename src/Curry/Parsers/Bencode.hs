{-# LANGUAGE TemplateHaskell #-}

module Curry.Parsers.Bencode
    ( BValue(..)
    , _BString
    , _BInt
    , _BList
    , _BDict
    , parseBValue
    , parseBString
    , parseBInt
    , parseBList
    , parseBDict
    , readInfoHash
    , writeBValue
    ) where

import           Curry.Parsers.Common

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Digest.SHA1
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
            | BDict [(String, BValue)]
            deriving Show

----------------------------------------
-- PARSERS
----------------------------------------

-- Parse a Bencoded value
parseBValue :: Parser BValue
parseBValue =  BString <$> parseBString
           <|> BInt    <$> parseBInt
           <|> BList   <$> parseBList
           <|> BDict   <$> parseBDict

-- Parse a Bencoded string
parseBString :: Parser B.ByteString
parseBString = decimal <* char ':' >>= take

-- Parse a Bencoded integer
parseBInt :: Parser Integer
parseBInt = parseMid 'i' $ signed decimal

-- Parse a Bencoded list
parseBList :: Parser [BValue]
parseBList = parseMid 'l' $ many' parseBValue

-- Parse a Bencoded dictionary
parseBDict :: Parser [(String, BValue)]
parseBDict = parseADict parseBValue

-- Parse a list of (key,value)'s according to a parser for values
-- (generalized because used both in parseBVal and rawDict)
parseADict :: Parser a -> Parser [(String, a)]
parseADict = parseMid 'd' . many1 . liftA2 (,) (C.unpack <$> parseBString)

-- Parse the between start and 'e'
parseMid :: Char -> Parser a -> Parser a
parseMid start middle = char start *> middle <* char 'e'

----------------------------------------
-- READERS
----------------------------------------

-- Extract raw bytestring of info key (if it exists), and calculate infohash
readInfoHash :: B.ByteString -> Maybe Word160
readInfoHash = fmap (hash . B.unpack . fst)
             . (mkReader (parseADict (match parseBValue)) >=> lookup "info")

----------------------------------------
-- WRITERS
----------------------------------------

writeBValue :: BValue -> B.ByteString
writeBValue (BString bytes ) = C.pack (show (B.length bytes) ++ ":") `B.append` bytes
writeBValue (BInt    n     ) = surround 'i' . C.pack $ show n
writeBValue (BList   bvals ) = surround 'l' . B.concat $ map writeBValue bvals
writeBValue (BDict   assocs) = surround 'd' $ B.concat
    [ C.pack (show (length key) ++ ':' : key) `B.append` writeBValue bval
    | (key, bval) <- assocs
    ]

surround :: Char -> B.ByteString -> B.ByteString
surround = (.) (`C.snoc` 'e') . C.cons

----------------------------------------
-- TEMPLATE HASKELL
----------------------------------------

makePrisms ''BValue
