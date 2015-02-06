module THP where

import           Torrent
import           Bencode

import           Control.Monad

import           Data.Bits
import           Data.Char
import           Data.Word
import           Data.Digest.SHA1
import           Data.List
import           Data.Maybe

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import           Network.Wreq

test :: IO ()
test = do
    file <- B.readFile "/home/nick/stuff/slackware-14.1-install-dvd.torrent"
    let Just t = torrentize file
        url =  mkURL $ initTHP $ t
    print url
    resp <- get url
    print resp


data THPrq = THPrq { tracker   :: String
                   , peer_id   :: Word160
                   , pport     :: Word
                   , key       :: Word160
                   , info_hash :: Word160
                   , event     :: Maybe TEvent
                   , status    :: TStatus
                   }

data TStatus = TStatus { uploaded   :: Integer
                       , downloaded :: Integer
                       , left       :: Integer
                       }

data TEvent = Started | Stopped | Completed

initPeerId = Word160 52395 54325 54321 64352 6543
initPort = 7000

initTHP :: Torrent -> THPrq
initTHP Torrent { announce  = announce'
                , pieceLen  = pieceLen'
                , fileStuff = fileStuff'
                , pieces    = pieces'
                , infoHash  = infoHash'
                }
  = THPrq { tracker   = announce'
          , peer_id   = initPeerId
          , pport     = initPort
          , key       = initPeerId
          , info_hash = infoHash'
          , event     = Just Started
          , status    = TStatus { uploaded   = 0
                                , downloaded = 0
                                , left       = case fileStuff'
                                               of   Left n   -> n
                                                    Right ns -> sum ns
                                }
          }

mkURL :: THPrq -> String
mkURL THPrq { tracker   = tracker'
            , peer_id   = peer_id'
            , pport     = pport'
            , key       = key'
            , info_hash = info_hash'
            , event     = event'
            , status    = TStatus { uploaded   = uploaded'
                                  , downloaded = downloaded'
                                  , left       = left'
                                  }
            } = tracker' ++ "?" ++ intercalate "&"
                   [ "info_hash="  ++ encode160 info_hash'
                   , "peer_id="    ++ encode160 peer_id'
                   , "port="       ++ show pport'
                   , "key="        ++ encode160 key'
                   , "uploaded="   ++ show uploaded'
                   , "downloaded=" ++ show downloaded'
                   , "left="       ++ show left'
                   , fromMaybe "" (fmap (("event=" ++) . encodeEvent) event')
                   ]

-- Encodings

encodeEvent :: TEvent -> String
encodeEvent Started   = "started"
encodeEvent Stopped   = "stopped"
encodeEvent Completed = "completed"

encode160 :: Word160 -> String
encode160 (Word160 a b c d e) = [a, b, c, d, e] >>= chunk32 >>= encode8

encode8 :: Word8 -> String
encode8 byte = ['%', toHexHalf $ shiftR byte 4, toHexHalf $ byte .&. 15]

toHexHalf :: Word8 -> Char
toHexHalf = genericIndex "0123456789ABCDEF"

chunk32 :: Word32 -> [Word8]
chunk32 x = map fromIntegral [ shiftR x 24
                             , shiftR x 16
                             , shiftR x  8
                             , x
                             ]
