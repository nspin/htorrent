module THP ( contact
           ) where

import Torrent
import Bencode

import Data.Maybe
import Numeric
import Control.Monad
import           Data.List
import           Data.Bits
import           Data.Char
import           Data.Word
import           Data.Digest.SHA1

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import           Network.HTTP.Base
import           Network.HTTP
import           Network.URI

test :: IO ()
test = do
    file <- B.readFile "/home/nick/stuff/slackware-14.1-install-dvd.torrent"
    let t = torrentize file
    case t of
        Nothing -> print "nooooooooooooooo"
        Just Torrent { announce  = announce'
                     , pieceLen  = pieceLen'
                     , fileStuff = Right fileStuf'
                     , pieces    = pieces'
                     , infoHash  = infoHash'
                     } -> contact
                           (C.unpack announce')
                           infoHash'
                           (TInfo {peer_id = Word160 432 432 12 35 342, rport = 7000}) 
                           (TStatus {uploaded = 0, downloaded = 0, left = negate (sum fileStuf')})
                           (Just Started)
            

data TInfo = TInfo { peer_id :: Word160
                   , rport   :: Word
                   }

data TStatus = TStatus { uploaded   :: Word
                       , downloaded :: Word
                       , left       :: Int
                       }

data TEvent = Started | Stopped | Completed

encodeEvent :: TEvent -> String
encodeEvent Started   = "started"
encodeEvent Stopped   = "stopped"
encodeEvent Completed = "completed"

contact :: String -> Word160 -> TInfo -> TStatus -> Maybe TEvent -> IO ()
contact tracker
        info
        TInfo { peer_id = peer_id'
              , rport   = rport'
              }
        TStatus { uploaded   = uploaded'
                , downloaded = downloaded'
                , left       = left'
                }
        event
  = do
    print uri
    resp <- simpleHTTP $ getRequest uri
    print resp
  where uri = tracker ++ "?" ++ intercalate "&"
            [ "info_hash=" ++ encode160 info
            , "peer_id="   ++ encode160 peer_id'
            , "port=" ++ show rport'
            , "uploaded=" ++ show uploaded'
            , "downloaded=" ++ show downloaded'
            , "left=" ++ show left'
            , fromMaybe "" (liftM (("event=" ++) . encodeEvent) event)
            ]

encode160 :: Word160 -> String
encode160 (Word160 a b c d e) = map toUpper ([a, b, c, d, e] >>= chunk32 >>= (\y -> let x = showHex y "" in '%' : (if length x == 1 then '0':x else x)))

chunk32 :: Word32 -> [Word8]
chunk32 x = map fromIntegral [ shiftR x 24
                             , shiftR x 16
                             , shiftR x  8
                             , x
                             ]
