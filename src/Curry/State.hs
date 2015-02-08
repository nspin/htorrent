module Curry.State
    ( ChuteIn
    , ChuteOut
    , newChutes
    , putChute
    , takeChute
    , Config
    , GlobalEnv
    , CommEnv
    , CommSt
    , BrainEnv
    , BrainSt
    ) where

import           Curry.Parsers.Torrent

import           Control.Concurrent.MVar
import           Control.Concurrent.MVar.ReadOnly
import           Control.Concurrent.MVar.WriteOnly
import qualified Data.ByteString as B
import qualified Data.Map as M
import           Network.Socket
import           System.IO

----------------------------------------
-- CHUTES
----------------------------------------

-- Simple wrappers around an MVar
data ChuteIn  a = ChuteIn  (MVar a) deriving Show
data ChuteOut a = ChuteOut (MVar a) deriving Show

newChutes :: IO (ChuteIn a, ChuteOut a)
newChutes = do
    mvar <- newMVar []
    return (ChuteIn mvar, ChuteOUt mvar)

-- These are thread safe because ALL threads modifying the wrapped mvars
-- will use a single take and single put, so they are guarenteed to be atomic.

putChute :: ChuteIn a -> a -> IO ()
putChute (ChuteIn mvar) x = do
    xs <- takeMVar mvar
    putMVar mvar (x:xs)

takeChute :: ChuteOut a -> IO [a]
takeChute (ChuteOut mvar) = do
    xs <- takeMVar mvar
    putMVar mvar []
    return xs

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
    , commUps    :: ChuteIn Integer
    , commDowns  :: ChuteIn Integer
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
    { peerOut    :: ChuteIn Peer
    , brainUps   :: ChuteOut Integer
    , brainDowns :: ChuteOut Integer
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
    , up      :: ReadOnlyMVar Integer
    , has     :: ReadOnlyMVar (M.Map Integer Bool)
    , chunks  :: ChuteIn [(Chunk, B.ByteString)]
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

-- To allow types with MVars to allow show (which will only be
-- used for debugging)

instance Show (MVar a) where
    show _ = "(an mvar exists here)"
