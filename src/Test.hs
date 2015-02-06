module Test where

import           Curtis.Track.Torrent
import           Curtis.Track.THP
import           Curtis.Bencode
import           Curtis.Common

import qualified Data.ByteString as B
import           Data.Digest.SHA1
import           Network.Wreq

test :: IO ()
test = do
    file <- B.readFile "/home/nick/stuff/slackware-14.1-install-dvd.torrent"
    let Just (MetaInfo t h) = getMeta file
    print t

-- initPeerId = Word160 52395 54325 54321 64352 6543
-- initPort = 7000

-- test :: IO ()
-- test = do
--     file <- B.readFile "/home/nick/stuff/slackware-14.1-install-dvd.torrent"
--     let Just t = torrentize file
--         url =  mkURL $ initTHP initPeerId initPort t
--     print url
--     resp <- get url
--     print resp

-- initTHP :: Word160 -> Word -> Torrent -> THPrq
-- initTHP initPeerId initPort Torrent
--     { announce  = announce'
--     , pieceLen  = pieceLen'
--     , fileStuff = fileStuff'
--     , pieces    = pieces'
--     , infoHash  = infoHash'
--     }
--   = THPrq { tracker   = announce'
--           , peer_id   = initPeerId
--           , pport     = initPort
--           , key       = initPeerId
--           , info_hash = infoHash'
--           , event     = Just Started
--           , status    = TStatus { uploaded   = 0
--                                 , downloaded = 0
--                                 , left       = case fileStuff'
--                                                of   Left n   -> n
--                                                     Right ns -> sum ns
--                                 }
--           }
