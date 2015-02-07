module Test where

import           Curtis.Tracker.Torrent
import           Curtis.Tracker.THP
import           Curtis.Bencode
import           Curtis.Common

import           Data.List
import qualified Data.ByteString as B
import           Network.Wreq

-- test :: IO ()
-- test = do
--     file <- B.readFile "/home/nick/stuff/slackware-14.1-install-dvd.torrent"
--     let Just (MetaInfo t h) = getMeta file
--         Torrent { announce = a
--                 , infoStuff = InfoStuff { piece_length = len
--                                         , pieces = n
--                                         }
--                 } = t
--         req = TRequest { tracker = a
--                        , info_hash = h
--                        , peer_id = idX
--                        , pport = portX
--                        , status = TStatus { uploaded = 0
--                                           , downloaded = 0
--                                           , left = len * genericLength n
--                                           }
--                        , compact = False
--                        , no_peer_id = False
--                        , event = Just Started
--                        , ip = Nothing
--                        , numwant = Nothing
--                        , key = Just idX
--                        , trackerid = Nothing
--                        }
--     print req
--     resp <- getTRespTEST req
--     print resp

test :: IO ()
test = do
    file <- B.readFile "/home/nick/stuff/slackware-14.1-install-dvd.torrent"
    let Just (MetaInfo t h) = getMeta file
        Torrent { announce = a
                , infoStuff = InfoStuff { piece_length = len
                                        , pieces = n
                                        }
                } = t
        req = TRequest { tracker = a
                       , info_hash = h
                       , peer_id = idX
                       , pport = portX
                       , status = TStatus { uploaded = 0
                                          , downloaded = 0
                                          , left = len * genericLength n
                                          }
                       , compact = False
                       , no_peer_id = False
                       , event = Just Started
                       , ip = Nothing
                       , numwant = Nothing
                       , key = Just idX
                       , trackerid = Nothing
                       }
    getTResp req >>= print

idX = B.pack [ 54, 43, 32, 33
             , 33, 33, 33, 33
             , 54, 54, 54, 54
             , 76, 76, 76, 67
             , 32, 32, 32, 32
             ]
portX = 7000

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
