module Test where

import           Curtis.Tracker.Torrent
import           Curtis.Tracker.THP
import           Curtis.Bencode
import           Curtis.Common

import           Control.Monad

import           Data.List
import qualified Data.ByteString as B
import           Network.Wreq

-- initial test values

idX = B.pack [ 54, 43, 32, 33
             , 33, 33, 33, 33
             , 54, 54, 54, 54
             , 76, 76, 76, 67
             , 32, 32, 32, 32
             ]
portX = 7000

test :: String -> IO ()
test = B.readFile >=> \file ->
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
    in getTResp req >>= print
