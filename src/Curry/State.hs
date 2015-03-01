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
-- OUR MONAD STACK (likely to change as
-- I decide to make the concurrency
-- control more granular.
----------------------------------------

type Curry = ReaderT Environment

----------------------------------------
-- COMMON TO THE ENTIRE INSANCE
----------------------------------------

data Environment = Environment
    { config    :: Config
    , identity  :: Identity
    , metaInfo  :: MetaInfo
    , currPiece :: TVar Map Chunk B.ByteString
    , pieceMap  :: TVar (M.Map Integer (Maybe  Handle))
    , peers     :: TVar [Peer]
    } deriving Show

data Config = Config
    { minPeers :: Integer
    , maxPeers :: Integer
    } deriving Show

data Identity = Identity
    { port       :: Integer -- port listening on
    , pid        :: B.ByteString -- out peer id (random)
    , key        :: B.ByteString -- our key (random)
    } deriving Show

data Chunk = Chunk
    { index :: Integer
    , start :: Integer
    , end   :: Integer
    } deriving (Show, Eq, Ord)

-- Information about a specific peer. Always exists in an MVar
data Peer = Peer
    { socket  :: Socket
    , theirID :: B.ByteString -- out peer id (random)
    , mut     :: TVar MutPeer
    } deriving Show

data MutPeer = MutPeer
    { status  :: Status
    , has     :: (M.Map Integer Bool)
    , up      :: Integer
    , down    :: Integer
    } deriving Show

data Status = Status
    { choked      :: Bool
    , choking     :: Bool
    , interesting :: Bool
    , interested  :: Bool
    } deriving Show

----------------------------------------
-- STATES FOR SPECIFIC THREADS (as the
-- codebase grows, these will be moved)
----------------------------------------

data CommSt = CommSt
    { trackerID   :: B.ByteString -- our tracker id
    , interval    :: Integer -- from tracker
    , minIntervel :: Integer -- from tracker
    }

data BrainSt = BrainSt
    { pieceNum   :: Integer
    , piecePart  :: M.Map Chunk B.ByteString
    , peers      :: MVar [Peer]
    } deriving Show

----------------------------------------
-- CETERA
----------------------------------------

-- To allow types with MVars to allow show (which will only be
-- used for debugging)

instance Show (MVar a) where
    show _ = "(an mvar exists here)"
