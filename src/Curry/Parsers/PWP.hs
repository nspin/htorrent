module Curry.Parsers.PWP
    ( Handshake(..)
    , ourProtocol
    , Message(..)
    , getShake
    , getMsg
    , mkShake
    , mkMsg
    ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as P
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
    { protocolHS :: String
    , infoHashHS :: B.ByteString
    , peerIdHS   :: B.ByteString
    }

ourProtocol :: String
ourProtocol = "BitTorrent protocol"

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
-- GETTERS
----------------------------------------

getShake :: B.ByteString -> Either String Handshake
getShake = eitherResult . parse parseShake

getMsg :: B.ByteString -> Either String Message
getMsg = eitherResult . parse parseMsg

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
                5 -> Bitfield <$> fmap unBitField takeByteString
                6 -> liftA3 Request bigEnd bigEnd bigEnd
                7 -> liftA3 Piece bigEnd bigEnd takeByteString
                8 -> liftA3 Cancel bigEnd bigEnd bigEnd

----------------------------------------
-- MAKERS
----------------------------------------

mkShake :: Handshake -> B.ByteString
mkShake (Handshake pcol infhash myid) = B.concat
    [ B.singleton (fromInteger $ genericLength pcol)
    , C.pack pcol
    , B.replicate 8 0
    , infhash
    , myid
    ]


mkMsg :: Message -> B.ByteString
mkMsg msg = unInt (toInteger $ B.length body) `B.append` body
  where
    body = case msg of
        Keepalive      -> B.empty
        Choke          -> B.singleton 0
        Unchoke        -> B.singleton 1
        Interested     -> B.singleton 2
        Bored          -> B.singleton 3
        Have     x     -> 4 `B.cons` unInt x
        Bitfield x     -> 5 `B.cons` bitField x
        Request  x y z -> 6 `B.cons` (B.concat . map unInt) [x, y, z]
        Piece    x y z -> 7 `B.cons` B.concat [unInt x, unInt y, z]
        Cancel   x y z -> 8 `B.cons` (B.concat . map unInt) [x, y, z]

----------------------------------------
-- HELPERS
----------------------------------------

-- parse a 4-bit big-endian integer
bigEnd = (sum . zipWith (*) (iterate (* 256) 1) . map fromIntegral . reverse . B.unpack) <$> take 4

unInt :: Integer -> B.ByteString
unInt int = B.pack [ fromIntegral $ 255 .&. shiftR int part
                   | part <- [24, 16, 8, 0]
                   ]

bitField :: M.Map Integer Bool -> B.ByteString
bitField = B.pack . unBitList . map snd . M.toList

unBitField :: B.ByteString -> M.Map Integer Bool
unBitField bits = M.fromList . zip [0..] . concatMap bitList $ B.unpack bits

bitList :: Word8 -> [Bool]
bitList w = map (testBit w) [0..7]

unBitList :: [Bool] -> [Word8]
unBitList = map ( foldl (.|.) 0
                . map (bit . fst)
                . filter snd
                . zip [7, 6..]
                ) . chunksOf 8
