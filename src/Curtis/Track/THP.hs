module Curtis.Track.THP
    ( THPrq
    , TStatus
    , TEvent
    , initTHP
    , mkURL
    ) where

import           Curtis.Bencode
import           Curtis.Track.Torrent
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

initTHP :: Word160 -> Word -> Torrent -> THPrq
initTHP initPeerId initPort Torrent
    { announce  = announce'
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

encodeEvent :: TEvent -> String
encodeEvent Started   = "started"
encodeEvent Stopped   = "stopped"
encodeEvent Completed = "completed"
