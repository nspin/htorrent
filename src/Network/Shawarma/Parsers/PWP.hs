{-# LANGUAGE RecordWildCards, FlexibleInstances #-}

module Network.Shawarma.Parsers.PWP
    ( Handshake(..)
    , Message(..)
    , ExtMsg
    , Context(..)
    , parseShake
    , parseMsg
    , writeShake
    , writeMsg
    ) where

import           Network.Shawarma.Prelude
import           Network.Shawarma.Parsers.Bencode
import           Network.Shawarma.Parsers.Word

import           Control.Applicative
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (anyChar, string)
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.List hiding (take)
import           Data.List.Split
import qualified Data.Map as M
import           Data.Word
import           Prelude hiding (take)

----------------------------------------
-- TYPES
----------------------------------------

data Handshake = Handshake
    { someCtxt :: Context
    , someHash :: Word160
    , someId   :: Word160
    } deriving Show

instance Eq Handshake where
    a == b = someHash a == someHash b

-- Not currenty implemented
data Context = Context
    { extprot :: Bool
    } deriving Show

data Message = Keepalive
             | Choke
             | Unchoke
             | Interested
             | Bored
             | Have Word32
             | Bitfield PieceMap
             | Request (Chunk Word32)
             | Piece (Chunk B.ByteString)
             | Cancel (Chunk Word32)
             | Port Word16
             | Extended ExtMsg
             deriving Show

data ExtMsg = ExtShake BDict
            deriving Show

----------------------------------------
-- PARSERS
----------------------------------------

parseShake :: Parser Handshake
parseShake = word8 19 *> string "BitTorrent Protocl" *>
             Handshake <$> (fmap fromIntegral anyWord8 >>= flip count anyChar)
                       <*> readCtxt <$> take 8
                       <*> parse160
                       <*> parse160

readCtxt :: B.ByteString -> Context
readCtxt bytes = Context { extprot = index bytes 5 `testBit` 10
                         }

-- This is sorta lame
parseMsg :: Context -> Context -> Parser Message
parseMsg us them = mkReader parseBody <$?> (parse32 >>= (take . fromInteger))

parseBody :: Context -> Context -> Parser Message
parseBody us them = endOfInput *> return Keepalive <|> do
    msgId <- anyWord8
    case msgId of
        0  -> return Choke
        1  -> return Unchoke
        2  -> return Interested
        3  -> return Bored
        4  -> Have <$> parse32
        5  -> Bitfield <$> readBitField <$> takeByteString
        6  -> liftA3 (((Request .).) . Chunk) parse32 parse32 parse32
        7  -> liftA3 (((Piece   .).) . Chunk) parse32 parse32 takeByteString
        8  -> liftA3 (((Cancel  .).) . Chunk) parse32 parse32 parse32
        20 -> perhaps (extprot $ extprot us && extprot them) $ Extended <$> parseExt us them
        _  -> empty

parseExt :: Context -> Context -> Parser ExtMsg
parseExt us them = do
    msgId <- anyWord8
    case msgId of
        0 -> ExtShake <$> parseBDict
        _ -> empty

----------------------------------------
-- MAKERS
----------------------------------------

writeShake :: Handshake -> B.ByteString
writeShake Handshake{..} = (19 `B.cons` C.pack "BitTorrent Protocol")
    `B.append` writeCtxt someCtxt
    `B.append` write160 someHash
    `B.append` write160 someId

writeCtxt :: Context -> B.ByteString
writeCtxt Context{..} = B.pack $ map (foldl setBit 0)
    [ []
    , []
    , []
    , []
    , []
    , [10]
    , []
    , []
    ]

-- Makes a lenght-prefixed message
writeMsg :: Context -> Context -> Message -> B.ByteString
writeMsg us them msg = (write32 . fromIntegral $ B.length body) `B.append` body
  where body = writeBody us them msg

writeBody :: Context -> Context -> Message -> B.ByteString

writeBody _ _ Keepalive  = B.empty
writeBody _ _ Choke      = B.singleton 0
writeBody _ _ Unchoke    = B.singleton 1
writeBody _ _ Interested = B.singleton 2
writeBody _ _ Bored      = B.singleton 3

writeBody _ _ (Have     x) = 4 `B.cons` write32 x
writeBody _ _ (Bitfield x) = 5 `B.cons` writeBitField x

writeBody _ _ (Request Chunk{..}) = 6 `B.cons` (B.concat . map write32) [index, start, body]
writeBody _ _ (Piece   Chunk{..}) = 7 `B.cons` B.concat [write32 index, write32 start, body]
writeBody _ _ (Cancel  Chunk{..}) = 8 `B.cons` (B.concat . map write32) [index, start, body]

----------------------------------------
-- HELPERS
----------------------------------------

writeBitField :: PieceMap -> B.ByteString
writeBitField = B.pack . unMkBitList . map snd . M.toList

readBitField :: B.ByteString -> PieceMap
readBitField bits = M.fromList . zip [0..] . concatMap mkBitList $ B.unpack bits

mkBitList :: Word8 -> [Bool]
mkBitList w = map (testBit w) [7,6..0] -- right?

unMkBitList :: [Bool] -> [Word8]
unMkBitList = map ( foldl (.|.) 0
                . map (bit . fst)
                . filter snd
                . zip [7, 6..]
                ) . chunksOf 8

----------------------------------------
-- CETERA
----------------------------------------

merge
