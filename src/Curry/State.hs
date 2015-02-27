module Curry.State where

import           Curry.Parsers.Torrent

import           Control.Concurrent.Chan
import           Control.Concurrent.Chan.ReadOnly
import           Control.Concurrent.Chan.WriteOnly
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

data GlobalEnv = GlobalEnv
    { metaInfo :: MetaInfo
    }

----------------------------------------
-- SPECIFIC TO COMMUNICATION THREADS
----------------------------------------

-- Environment for a variable
data CommEnv = CommEnv
    { port       :: Integer
    , pid        :: B.ByteString
    , key        :: B.ByteString
    , pids       :: MVar [B.ByteString] -- peers that have been connected to so far
    , commUps    :: ReadOnlyChan Integer
    , commDowns  :: ReadOnlyChan Integer
    } deriving Show

data CommSt = CommSt
    { trackerID   :: B.ByteString
    , interval    :: Integer
    , minIntervel :: Integer
    , downloaded  :: Integer
    , uploaded    :: Integer
    }

----------------------------------------
-- SPECIFIC TO BRAIN THREADS
----------------------------------------

data BrainEnv = BrainEnv
    { peerOut    :: ReadOnlyChan Peer
    , brainUps   :: WriteOnlyChan Integer
    , brainDowns :: WriteOnlyChan Integer
    }

data BrainSt = BrainSt
    { pieceNum   :: Integer
    , piecePart  :: M.Map Chunk B.ByteString
    , pieceMap   :: M.Map Integer (Either B.ByteString  Handle)
    , peers      :: [Peer]
    } deriving Show

-- Information about a specific peer. Always exists in an MVar
data Peer = Peer
    { socket  :: Socket
    , status  :: MVar Status
    , up      :: ReadOnlyMVar Integer
    , has     :: ReadOnlyMVar (M.Map Integer Bool)
    , chunks  :: ReadOnlyChan [(Chunk, B.ByteString)]
    , close   :: MVar ()
    } deriving Show

data Chunk = Chunk
    { index :: Integer
    , start :: Integer
    , end   :: Integer
    } deriving (Show, Eq, Ord)

data Status = Status
    { choked      :: Bool
    , choking     :: Bool
    , inerested   :: Bool
    , interesting :: Bool
    } deriving Show

----------------------------------------
-- CETERA
----------------------------------------

-- query for acid state
-- To allow types with MVars and Chans to allow show (which will only be
-- used for debugging)

instance Show (MVar a) where
    show _ = "(an mvar exists here)"

instance Show (ReadOnlyMVar a) where
    show _ = "(a readonly mvar exists here)"

instance Show (WriteOnlyMVar a) where
    show _ = "(a writeonly mvar exists here)"

instance Show (Chan a) where
    show _ = "(an mvar exists here)"

instance Show (ReadOnlyChan a) where
    show _ = "(a readonly chan exists here)"

instance Show (WriteOnlyChan a) where
    show _ = "(a writeonly chan exists here)"
