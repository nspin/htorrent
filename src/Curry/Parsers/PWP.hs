module Curry.Parsers.PWP
    ( Handshake(..)
    , Message(..)
    , getShake
    , getMsg
    , mkShake
    , mkMsg
    ) where

import           Control.Applicative
import           Data.Bits
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.List hiding (take)
import           Data.Word
import           Prelude hiding (take)

----------------------------------------
-- TYPES
----------------------------------------

data Handshake = Handshake
    { protocolHS :: String
    , infoHashHS :: B.ByteString
    , peerIDHS   :: B.ByteString
    }

data Message = Keepalive
             | Choke
             | Unchoke
             | Interested
             | Bored
             | Have Int
             | Bitfield B.ByteString
             | Request Int Int Int
             | Piece Int Int B.ByteString
             | Cancel Int Int Int
             deriving Show

----------------------------------------
-- GETTERS
----------------------------------------

getShake :: B.ByteString -> Maybe Handshake
getShake = maybeResult . parse parseShake

getMsg :: B.ByteString -> Maybe Message
getMsg = maybeResult . parse parseMsg

parseShake = liftA3 Handshake (C.unpack <$> (fmap fromIntegral anyWord8 >>= take)) (take 20) (take 20)

-- TODO: check with len
parseMsg = do
    len <- bigEnd -- ^^^
    case len of
        0 -> return Keepalive
        _ -> do
            msgID <- anyWord8
            case msgID of
                0 -> return Choke
                1 -> return Unchoke
                2 -> return Interested
                3 -> return Bored
                4 -> Have <$> bigEnd
                5 -> Bitfield <$> takeByteString
                6 -> liftA3 Request bigEnd bigEnd bigEnd
                7 -> liftA3 Piece bigEnd bigEnd takeByteString
                8 -> liftA3 Cancel bigEnd bigEnd bigEnd

----------------------------------------
-- MAKERS
----------------------------------------

mkShake :: B.ByteString -> B.ByteString -> B.ByteString
mkShake myid infhash = B.concat [ B.singleton 19
                                , C.pack "BitTorren protocol"
                                , B.replicate 8 0
                                , infhash
                                , myid
                                ]


mkMsg :: Message -> B.ByteString
mkMsg msg = unInt (B.length body) `B.append` body
  where
    body = case msg of
        Keepalive      -> B.empty
        Choke          -> B.singleton 0
        Unchoke        -> B.singleton 1
        Interested     -> B.singleton 2
        Bored          -> B.singleton 3
        Have     x     -> 4 `B.cons` unInt x
        Bitfield x     -> 5 `B.cons` x
        Request  x y z -> 6 `B.cons` (B.concat . map unInt) [x, y, z]
        Piece    x y z -> 7 `B.cons` B.concat [unInt x, unInt y, z]
        Cancel   x y z -> 8 `B.cons` (B.concat . map unInt) [x, y, z]

----------------------------------------
-- HELPERS
----------------------------------------

-- parse a 4-bit big-endian integer
bigEnd = (sum . zipWith (*) (iterate (* 256) 1) . map fromIntegral . reverse . B.unpack) <$> take 4

unInt :: Int -> B.ByteString
unInt int = B.pack [ fromIntegral $ 255 .&. shiftR int part 
                   | part <- [24, 16, 8, 0]
                   ]
