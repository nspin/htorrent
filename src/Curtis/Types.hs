module Curtis.Types where

import qualified Data.ByteString as B
import qualified Data.Map as M
import Control.Concurrent.MVar
import Data.Word
import Network.Socket

----------------------------------------
-- VARIOUS SORTS OF STATE
----------------------------------------

-- Each is named according to/parameterized over
-- the following qualifiers:
--      G = global (common to all instances of the client)
--      D = download (specific to a certain torrent)
--      P = persistant (using acid state)
--      T = temporary (dies with instance of client)

-- Not yet implemented. Will contain config of some sort.
data GP = GP
    { ip  :: String
    , key :: B.ByteString
    }

data GT = GT
    { myId   :: B.ByteString
    , myPort :: Int
    } deriving Show

data DT = DT
    { uploaded  :: Int
    , downoaded :: Int
    , left      :: Int
    , peers     :: [MVar Peer]
    } deriving Show

data DP = DP
    { metainfo   :: MetaInfo
    , complete   :: M.Map Int        B.ByteString
    , incomplete :: M.Map (Int, Int) B.ByteString
    } deriving Show

-- Information about a specific peer. Always exists in an MVar
data Peer = Peer
    { idAddr   :: String
    , idPort   :: String
--  , id       :: PeerID -- necessary? (yes for tracker thread decisions)
    , sock     :: Socket -- necessary?
    , relation :: Relation
    , status   :: (M.Map Integer Bool)
    , up       :: Word
    , down     :: Word
    , chan     :: Command
    } deriving Show

data Relation = Relation
    { choked      :: Bool
    , choking     :: Bool
    , inerested   :: Bool
    , interesting :: Bool
    } deriving Show

-- TODO
data Command = Command deriving Show

----------------------------------------
-- PEER WIRE PROTOCOL
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

----------------------------------------
-- COMMUNICATION WITH TRACKER
----------------------------------------

-- Tracker request
-- Trailing 0's are a temporary hack (ghci can't handle different
-- types with the same record labels (which simon peyton jones has
-- acknowledged is one of haskell's most obvious mistakes), and while
-- all of these types are in the same module, this is a problem. So,
-- until then, trailing 0's are the fix.
data Traq = Traq
    { tracker    :: String
    , info_hash0 :: B.ByteString
    , peer_id    :: B.ByteString
    , pport      :: Word
    , status0    :: OurStatus
    , event      :: Maybe OurEvent
    , ip         :: Maybe String
    , key        :: B.ByteString
    , trackerid  :: String
    } deriving Show

data OurStatus = OurStatus
    { uploaded0   :: Integer
    , downloaded0 :: Integer
    , left0       :: Integer
    } deriving Show

data OurEvent = Started | Stopped | Completed deriving Show

----------------------------------------
-- METAINFO
----------------------------------------

-- All of the information in(/about) a torrent file that curtis is,
-- at this point, capable of caring about.

data MetaInfo = MetaInfo
    { torrent   :: Torrent
    , info_hash :: B.ByteString
    } deriving Show

data Torrent = Torrent
    { announce      :: String
    , announce_list :: Maybe [String]
    , comment       :: Maybe String
    , created_by    :: Maybe String
    , creation_date :: Maybe Integer
    , encoding      :: Maybe String
    , info          :: Info
    } deriving Show

data Info = Info
    { piece_length :: Integer
    , pieces       :: [B.ByteString]
    , private      :: Bool
    , fileStuff    :: Either (FileInfo String) (String, [FileInfo [String]])
    } deriving Show

data FileInfo a = FileInfo
    { name   :: a
    , length :: Integer
    , md5sum :: Maybe B.ByteString
    } deriving Show

----------------------------------------
-- CETERA
----------------------------------------

-- A bencoded value.
-- Note that bdict keys are strings, not bytestrings (or, if you prefer,
-- that bstrings are bytestrings). These are different because bstrings
-- may be binary data, whereas (in all implementations I know of) keys

-- are always text. Storing keys as strings allows string literals to
-- be used in lookup (rather than packing for each lookup, which is not
-- very efficient).
data BValue = BString B.ByteString
            | BInt Integer
            | BList [BValue]
            | BDict [(String, BValue)]
            deriving Show

----------------------------------------
-- IGNORE
----------------------------------------

instance Show (MVar a) where
    show _ = "(an mvar exists here)"
