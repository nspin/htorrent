{-# LANGUAGE RecordWildCards, FlexibleInstances #-}

module Curry.Parsers.PWP
    ( Handshake(..)
    , Message(..)
    , Context(..)
    , parseShake
    , parseMsg
    , mkShake
    , mkMsg
    , checkMsg
    , merge
    ) where

import           Curry.Common

import           Control.Applicative
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (anyChar)
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import           Data.List hiding (take)
import           Data.List.Split
import qualified Data.Map as M
import           Data.Word
import           Prelude hiding (take)
import           System.IO.Unsafe

----------------------------------------
-- TYPES
----------------------------------------

data Handshake = Handshake
    { protocol :: String
    , someCtxt :: Context
    , someHash :: B.ByteString
    , someId   :: B.ByteString
    } deriving Show

instance Eq Handshake where
    a == b = protocol a == protocol b && someHash a == someHash b

-- Not currenty implemented
data Context = Context deriving Show
    -- { dht :: Bool
    -- , utp :: Bool
    -- , etc.
    -- }

data Message = Keepalive
             | Choke
             | Unchoke
             | Interested
             | Bored
             | Have Integer
             | Bitfield (M.Map Integer Bool)
             | Request (Chunk Integer)
             | Piece (Chunk B.ByteString)
             | Cancel (Chunk Integer)
             deriving Show

----------------------------------------
-- PARSERS
----------------------------------------

parseShake = Handshake <$> (anyWord8 >>= ((`count` anyChar) . fromIntegral))
                       <*> fmap ctxt (take 8)
                       <*> take 20
                       <*> take 20

ctxt :: B.ByteString -> Context
ctxt _ = Context

-- This is sorta lame
parseMsg = do
    len <- parseInt
    rest <- take $ fromInteger len
    -- unsafePerformIO $ print (maybeResult $ parse takeByteString rest) >> return (return ())
    case maybeResult (feed (parse parseBody rest) B.empty) of
        Nothing -> fail "done"
        Just msg -> return msg

-- parse a 4-byte big-endian integer
parseInt = (sum . zipWith (*) (iterate (* 256) 1) . map fromIntegral . reverse . B.unpack) <$> take 4

parseBody = endOfInput *> return Keepalive <|> do
    msgID <- anyWord8
    case msgID of
        0 -> return Choke
        1 -> return Unchoke
        2 -> return Interested
        3 -> return Bored
        4 -> Have <$> parseInt
        5 -> (Bitfield . unBitField) <$> takeByteString
        6 -> liftA3 (((Request .).) . Chunk) parseInt parseInt parseInt
        7 -> liftA3 (((Piece   .).) . Chunk) parseInt parseInt takeByteString
        8 -> liftA3 (((Cancel  .).) . Chunk) parseInt parseInt parseInt

----------------------------------------
-- MAKERS
----------------------------------------

mkShake :: Handshake -> B.ByteString
mkShake Handshake{..} = B.singleton 19 `B.append` C.pack protocol
                                       `B.append` context'
                                       `B.append` someHash
                                       `B.append` someId
  where context' = B.pack $ replicate 8 0

-- Makes a lenght-prefixed message
mkMsg :: Message -> B.ByteString
mkMsg msg = (B.singleton . fromInteger . toInteger $ B.length body) `B.append` body
  where body = mkBody msg

-- Turn an integer into a big-endian 4-bit bytestring.
-- Oversized ints are not handled.
mkInt :: Integer -> B.ByteString
mkInt int = B.pack [ fromIntegral $ 255 .&. shiftR int part
                   | part <- [24, 16, 8, 0]
                   ]

mkBody :: Message -> B.ByteString

mkBody Keepalive  = B.empty
mkBody Choke      = B.singleton 0
mkBody Unchoke    = B.singleton 1
mkBody Interested = B.singleton 2
mkBody Bored      = B.singleton 3

mkBody (Have     x) = 4 `B.cons` mkInt x
mkBody (Bitfield x) = 5 `B.cons` bitField x

mkBody (Request Chunk{..}) = 6 `B.cons` (B.concat . map mkInt) [index, start, body]
mkBody (Piece   Chunk{..}) = 7 `B.cons` B.concat [mkInt index, mkInt start, body]
mkBody (Cancel  Chunk{..}) = 8 `B.cons` (B.concat . map mkInt) [index, start, body]

----------------------------------------
-- UTILS (will grow with Context)
----------------------------------------

checkMsg :: Context -> Message -> Either String Message
checkMsg _ msg = Right msg

merge :: Context -> Context -> Context
merge _ = id

----------------------------------------
-- HELPERS
----------------------------------------

bitField :: M.Map Integer Bool -> B.ByteString
bitField = B.pack . unBitList . map snd . M.toList

unBitField :: B.ByteString -> M.Map Integer Bool
unBitField bits = M.fromList . zip [0..] . concatMap bitList $ B.unpack bits

bitList :: Word8 -> [Bool]
bitList w = map (testBit w) [7,6..0] -- right?

unBitList :: [Bool] -> [Word8]
unBitList = map ( foldl (.|.) 0
                . map (bit . fst)
                . filter snd
                . zip [7, 6..]
                ) . chunksOf 8
