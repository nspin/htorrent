module Curis.Types where

import Data.ByteString
import Data.Map

-- Our monad stack
type T = ReaderT Download (StateT (MVar Tate) IO)

-- progress is a map from piece number to pairs of start-end bites of needed blocks
data Tate = Tate { progress :: Map Integer [(Integer, Integer)]
                 , --peers    :: [MVar Peer]
                 } deriving Show

data Progress = Progress { pieces  :: Map Integer Bool
                         , current :: Integer
                         , cprog   :: [(Int, Int)]
                         , tstat   :: TStatus
                         } deriving Show

-- infohash, pid, then pieces
data Download = Download { tracker  :: String
                         , myd      :: B.ByteString
                         , metahash :: B.ByteString
                         , myport   :: Integer
                         , phashes  :: [B.ByteString]
                         } deriving Show

data Peer = Peer { id       :: PeerID -- necessary? (yes for tracker thread decisions)
                 , sock     :: Socket -- necessary?
                 , relation :: Relation
                 , status   :: (Map Integer Bool)
                 , up       :: Word
                 , down     :: Word
                 , cchan    :: Command
                 } deriving Show

-- addr, port
type PeerID = (String, String)

data Relation = Relation { choked      :: Bool
                         , choking     :: Bool
                         , inerested   :: Bool
                         , interesting :: Bool
                         } deriving Show

-- Peer Wire Protocol types

data Handshake = Handshake String B.ByteString B.ByteString

data Message = Keepalive
             | Choke
             | Unchoke
             | Intersted
             | Bored
             | Have Int
             | Bitfield B.ByteString
             | Request Int Int Int
             | Piece Int Int B.ByteString
             | Cancel Int Int Int
             deriving Show


-- A bencoded value
data BValue = BString B.ByteString
            | BInt Integer
            | BList [BValue]
            | BDict [(B.ByteString, BValue)]
            deriving Show

-- Communication with tracker

data TRequest = TRequest { tracker    :: String
                         , info_hash  :: B.ByteString
                         , peer_id    :: B.ByteString
                         , pport      :: Word
                         , status     :: TStatus
                         , event      :: Maybe TEvent
                         , ip         :: String
                         , key        :: B.ByteString
                         , trackerid  :: String
                         } deriving Show
  
  
data OurStatus = OurStatus { uploaded   :: Integer
                           , downloaded :: Integer
                           , left       :: Integer
                           } deriving Show

data OurEvent = Started | Stopped | Completed deriving Show

-- All of the information in(/about) a torrent file that curtis is,
-- at this point, capable of caring about.

data MetaInfo = MetaInfo Torrent B.ByteString deriving Show

data Torrent = Torrent
    { announce      :: String
    , announce_list :: Maybe [String]
    , comment       :: Maybe String
    , created_by    :: Maybe String
    , creation_date :: Maybe Integer
    , encoding      :: Maybe String
    , infoStuff     :: InfoStuff
    } deriving Show

data Info = Info
    { piece_length :: Integer
    , pieces       :: [B.ByteString]
    , private      :: Bool
    , files        :: Either (FileInfo String) (String, FileInfo [String])
    } deriving Show

data FileInfo a = FileInfo
    { name   :: a
    , length :: Integer
    , md5sum :: Maybe B.ByteString
    } deriving Show
