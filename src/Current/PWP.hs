module Current.PWP
    ( Handshake(..)
    , getShake
    , mkShake
    , Message(..)
    , getMsg
    , mkMsg
    ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.List
import           Data.Word

getHand :: B.ByteString -> Maybe Handshake
getHand = maybeResult . parse parseShake

getMsg :: B.ByteString -> Maybe Message
getMsg = maybeResult . parse parseMsg

parseHand = liftA3 Handshake (anyWord8 >>= P.take) (take 20) (take 20)

mkShake :: B.ByteString -> B.ByteString -> B.ByteString
mkShake myid infhash = concat [ B.pack 19
                              , pstr
                              , B.replicate 8 0
                              , infhash
                              , myid
                              ]

pstr :: B.ByteString
pstr = C.pack "BitTorrent protocol"

-- TODO: check with len
parseMsg = do
    len <- pwpInt -- ^^^
    case len of
        0 -> return Keepalive
        _ -> do
            msgID <- anyWord8
            case msgID of
                0 -> return Choke
                1 -> return Unchoke
                2 -> return Interested
                3 -> return Bored
                4 -> Have <$> pwpInt
                5 -> Bitfield <$> takeByteString
                6 -> liftA3 Request pwpInt pwpInt pwpInt
                7 -> liftA3 Piece pwpInt pwpInt takeByteString
                8 -> liftA3 Cancel pwpInt pwpInt pwpInt

-- parse a 4-bit big-endian integer
pwpInt = sum . zipWith (*) (iterate (* 256) 1) . map fromIntegral . reverse <$> take 4

unInt :: Int -> B.ByteString
unInt int = B.pack [ fromIntegral $ 255 .&. shiftR part int
                   | part <- [24, 16, 8, 0]
                   ]

mkShake :: Message -> B.ByteString
mkMsg msg = unInt (length body) `B.append` body
  where body = case msg of
    Keepalive      -> B.empty
    Choke          -> B.pack 0
    Unchoke        -> B.pack 1
    Intersted      -> B.pack 2
    Bored          -> B.pack 3
    Have     x     -> B.pack 4 `B.append` unInt x
    Bitfield x     -> B.pack 5 `B.append` x
    Request  x y z -> B.pack 6 `B.append` (B.concat . map unInt) [x, y, z]
    Piece    x y z -> B.pack 7 `B.append` B.concat [unInt x, unInt y, z]
    Cancel   x y z -> B.pack 8 `B.append` (B.concat . map unInt) [x, y, z]
