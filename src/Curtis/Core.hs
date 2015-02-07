module Curtis.Core ( download
                   ) where

import Data.Map

data Tate = Tate { peers :: [Peer]
                 , file  :: 
                 }

type PeerID = (String, String) -- addr, port

data Peer = Peer PeerID Relation (Map Integer Bool)

data Relation = Relation { choked      :: Bool
                         , choking     :: Bool
                         , inerested   :: Bool
                         , interesting :: Bool
                         }

download :: FilePath -- where to store pieces
         -> MVar [PeerID] -- up-to-date peer reports from tracker or dht
         -> MVar [Peer]
         -> IO
