module Curis.Types (

import Data.ByteString
import Data.Map

type T = ReaderT Download (StateT (MVar Tate) IO)


-- progress is a map from piece number to pairs of start-end bites of needed blocks
data Tate = Tate { progress :: Map Integer [(Integer, Integer)]
                 , --peers    :: [MVar Peer]
                 }

data Progress = Progress { pieces  :: Map Integer Bool
                         , current :: Integer
                         , cprog   :: [(Int, Int)]
                         , tstat   :: TStatus
                         }

-- infohash, pid, then pieces
data Download = Download { tracker  :: String
                         , myd      :: B.ByteString
                         , metahash :: B.ByteString
                         , myport   :: Integer
                         , phashes  :: [B.ByteString]
                         }

data Peer = Peer { id       :: PeerID -- necessary? (yes for tracker thread decisions)
                 , sock     :: Socket -- necessary?
                 , relation :: Relation
                 , status   :: (Map Integer Bool)
                 , up       :: Word
                 , down     :: Word
                 , cchan    :: Command
                 }

-- addr, port
type PeerID = (String, String)

data Relation = Relation { choked      :: Bool
                         , choking     :: Bool
                         , inerested   :: Bool
                         , interesting :: Bool
                         }

-- A bencoded value
data BValue = BString B.ByteString
            | BInt Integer
            | BList [BValue]
            | BDict [(B.ByteString, BValue)]
            deriving Show


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

data TRequest = TRequest { tracker    :: String
                         , info_hash  :: B.ByteString
                         , peer_id    :: B.ByteString
                         , pport      :: Word
                         , status     :: TStatus
                         , compact    :: Bool
                         , no_peer_id :: Bool
                         , event      :: Maybe TEvent
                         , ip         :: Maybe String
                         , numwant    :: Maybe Word
                         , key        :: Maybe B.ByteString
                         , trackerid  :: Maybe String
                         }
  deriving Show
  
  
data TStatus = TStatus { uploaded   :: Integer
                       , downloaded :: Integer
                       , left       :: Integer
                       }
  deriving Show

data TEvent = Started | Stopped | Completed
  deriving Show


data MetaInfo = MetaInfo Torrent B.ByteString deriving Show

-- All of the information in a torren file that curtis is,
-- at this point, capable of caring about. Curtis will grow,
-- though, so this will expand at some point.
data Torrent = Torrent
    { announce      :: String
    , announce_list :: Maybe [String]
    , comment       :: Maybe String
    , created_by    :: Maybe String
    , creation_date :: Maybe Integer
    , infoStuff     :: InfoStuff
    }
  deriving Show

data InfoStuff = InfoStuff
    { piece_length :: Integer
    , pieces       :: [B.ByteString]
    , private      :: Bool
    , fileStuff    :: Either OneFile ManyFiles
    }
  deriving Show

data OneFile = OneFile
    { nameO   :: String
    , lengthO :: Integer
    , md5sumO :: Maybe B.ByteString
    }
  deriving Show

data ManyFiles = ManyFiles
    { nameM  :: String -- semanticaly different from OneFile name, so seperate.
    , filesM :: [ManyFile]
    }
  deriving Show

data ManyFile = ManyFile
    { pathM  :: [String]
    , lengthM :: Integer
    , md5sumM :: Maybe B.ByteString -- (Word64, Word64)
    }
  deriving Show
