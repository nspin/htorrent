module Curry.State where

import           Curry.Parsers.Torrent

import           Control.Concurrent.MVar
import           Control.Concurrent.MVar.ReadOnly
import           Control.Concurrent.MVar.WriteOnly
import qualified Data.ByteString as B
import qualified Data.Map as M
import           Network.Socket
import           System.IO

----------------------------------------
-- COMMON TO THE ENTIRE INSANCE
----------------------------------------

data Config = Config
    { minPeers :: Integer
    , maxPeers :: Integer
    }

data GlobalEnv = GlobalEnv
    { metaInfo :: MetaInfo
    , pieceMap :: MVar (M.Map Integer (Maybe  Handle))
    , totalUp  :: CountView Integer -- tally of total downloaded
    }

----------------------------------------
-- SPECIFIC TO COMMUNICATION THREAD
----------------------------------------

-- Environment for a variable
data CommEnv = CommEnv
    { port       :: Integer -- port listening on
    , pid        :: B.ByteString -- out peer id (random)
    , key        :: B.ByteString -- our key (random)
    , pids       :: MVar [B.ByteString] -- peers that have been connected to so far
    , peerIn     :: ChuteIn Peer
    } deriving Show

data CommSt = CommSt
    { trackerID   :: B.ByteString -- our tracker id
    , interval    :: Integer -- from tracker
    , minIntervel :: Integer -- from tracker
    }

----------------------------------------
-- SPECIFIC TO FRIENDHSIP THREADS
----------------------------------------

data FriendEnv = FriendEnv
    { status'  :: MVar Status
    , has'     :: MCtrl (M.Map Integer Bool)
    , up'      :: CountCtrl Integer
    , down'    :: CountCtrl Integer
    , chunks'  :: ChuteOut [(Chunk, B.ByteString)]
    , totalUp' :: CountCtrl Integer -- tally of total uploaded (shared with
    }

----------------------------------------
-- SPECIFIC TO BRAIN THREADS
----------------------------------------

data BrainEnv = BrainEnv
    { peerOut    :: ChuteOut Peer
    , chunks     :: ChuteIn [(Chunk, B.ByteString)]
    }

data BrainSt = BrainSt
    { pieceNum   :: Integer
    , piecePart  :: M.Map Chunk B.ByteString
    , peers      :: [Peer]
    } deriving Show

-- Information about a specific peer. Always exists in an MVar
data Peer = Peer
    { socket  :: Socket
    , status  :: MVar Status
    , has     :: MView (M.Map Integer Bool)
    , up      :: CountView Integer
    , down    :: CountView Integer
    } deriving Show

data Chunk = Chunk
    { index :: Integer
    , start :: Integer
    , end   :: Integer
    } deriving (Show, Eq, Ord)

data Status = Status
    { choked      :: Bool
    , choking     :: Bool
    , interesting :: Bool
    , interested  :: Bool
    } deriving Show

----------------------------------------
-- CETERA
----------------------------------------

-- To allow types with MVars to allow show (which will only be
-- used for debugging)

instance Show (MVar a) where
    show _ = "(an mvar exists here)"
