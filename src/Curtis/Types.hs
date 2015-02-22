module Curis.Types where

import Data.ByteString
import Data.Map

----------------------------------------
-- VARIOUS SORTS OF STATE 
----------------------------------------

-- Each is named according to/parameterized over
-- the following qualifiers:

--      G = global (common to all instances of the client)
--      D = download (specific to a certain torrent)
--      P = persistant (using acid state)
--      T = temporary (dies with instance of client)

-- Not implemented atm. Will contain config of some sort.
data GP = GP

data GT = GT { myId :: B.ByteString
             , myPort :: Int
             }

data DT = DT { uploaded :: Int
             , downoaded :: Int
             , left :: Int
             , peers :: [MVar Peer]
             } deriving Show

data DP = DP { meta       :: MetaInfo
             , complete   :: M.Map Int        B.ByteString
             , incomplete :: M.Map (Int, Int) B.ByteString
             }

-- Information about a specific peer. Always exists in an MVar
data Peer = Peer { id       :: PeerID -- necessary? (yes for tracker thread decisions)
                 , sock     :: Socket -- necessary?
                 , relation :: Relation
                 , status   :: (Map Integer Bool)
                 , up       :: Word
                 , down     :: Word
                 , chan    :: Command
                 } deriving Show

-- addr, port
type PeerID = (String, String)

data Relation = Relation { choked      :: Bool
                         , choking     :: Bool
                         , inerested   :: Bool
                         , interesting :: Bool
                         } deriving Show

----------------------------------------
-- PEER WIRE PROTOCOL TYPES
----------------------------------------

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

----------------------------------------
-- COMMUNICATION WITH TRACKER
----------------------------------------

-- Tracker request
data Traq = Traq { tracker    :: String
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

----------------------------------------
-- METAINFO
----------------------------------------

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
