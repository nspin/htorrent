{-# LANGUAGE RecordWildCards, FlexibleInstances #-}

module Curry.Parsers.PWP
    ( Handshake(..)
    , Message(..)
    , Context(..)
    , parseShake
    , parseInt
    , parseMsg
    , mkShake
    , mkInt
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

----------------------------------------
-- TYPES
----------------------------------------

data Handshake = Handshake
    { protocol :: String
    , someCtxt :: Context
    , someId   :: B.ByteString
    , someHash :: B.ByteString
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
             | Request Integer Integer Integer
             | Piece Integer Integer B.ByteString
             | Cancel Integer Integer Integer
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

-- parse a 4-byte big-endian integer
parseInt = (sum . zipWith (*) (iterate (* 256) 1) . map fromIntegral . reverse . B.unpack) <$> take 4

parseMsg = endOfInput *> return Keepalive <|> do
    msgID <- anyWord8
    case msgID of
        0 -> return Choke
        1 -> return Unchoke
        2 -> return Interested
        3 -> return Bored
        4 -> Have <$> parseInt
        5 -> (Bitfield . unBitField) <$> takeByteString
        6 -> liftA3 Request parseInt parseInt parseInt
        7 -> liftA3 Piece parseInt parseInt takeByteString
        8 -> liftA3 Cancel parseInt parseInt parseInt

----------------------------------------
-- MAKERS
----------------------------------------

mkShake :: Handshake -> B.ByteString
mkShake Handshake{..} = B.singleton 19 `B.append` C.pack protocol
                                       `B.append` context'
                                       `B.append` someId
                                       `B.append` someHash
  where context' = B.pack $ replicate 8 0

-- Turn an integer into a big-endian 4-bit bytestring.
-- Oversized ints are not handled.
mkInt :: Integer -> B.ByteString
mkInt int = B.pack [ fromIntegral $ 255 .&. shiftR int part
                      | part <- [24, 16, 8, 0]
                      ]

mkMsg :: Message -> B.ByteString
mkMsg (Keepalive     ) = B.empty
mkMsg (Choke         ) = B.singleton 0
mkMsg (Unchoke       ) = B.singleton 1
mkMsg (Interested    ) = B.singleton 2
mkMsg (Bored         ) = B.singleton 3
mkMsg (Have     x    ) = 4 `B.cons` mkInt x
mkMsg (Bitfield x    ) = 5 `B.cons` bitField x
mkMsg (Request  x y z) = 6 `B.cons` (B.concat . map mkInt) [x, y, z]
mkMsg (Piece    x y z) = 7 `B.cons` B.concat [mkInt x, mkInt y, z]
mkMsg (Cancel   x y z) = 8 `B.cons` (B.concat . map mkInt) [x, y, z]

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
