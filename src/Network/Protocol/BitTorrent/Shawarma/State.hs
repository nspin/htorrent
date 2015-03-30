{-# LANGUAGE TemplateHaskell #-}

module Network.Protocol.BitTorrent.Shawarma.Types
    ( Env(..)
    , Config(..)
    , Peer(..)
    , Hist(..)
    , Status(..)
    -- Set-like channel
    , TShan
    , readTShan
    , writeTShan
    , rmvTShan
    -- Operations
    , opHas
    , setChoked
    , setInteresting
    , addUp
    , addDown
    ) where

import           Network.Protocol.BitTorrent.Shawarma.Common
import           Network.Protocol.BitTorrent.Shawarma.Parsers.PWP
import           Network.Protocol.BitTorrent.Shawarma.Parsers.THP
import           Network.Protocol.BitTorrent.Shawarma.Parsers.Torrent

import           Control.Concurrent.STM
import           Control.Lens
import qualified Data.ByteString as B
import qualified Data.Map as M
import           Data.UnixTime

----------------------------------------
-- GENERAL STATE TYPES
----------------------------------------

-- Overall download environment
data Env = Env
    { _config    :: Config
    , _whoami    :: Addr
    , _ourId     :: B.ByteString
    , _ourKey    :: B.ByteString
    , _metaInfo  :: MetaInfo
    , _peers     :: TVar [Peer]
    , _progress  :: TVar PieceMap
    , _gives     :: TShan ((Chunk B.ByteString), Peer)
    , _takes     :: TShan ((Chunk Integer     ), Peer)
    , _sayChan   :: TChan String
    } deriving Show

-- General static settings
data Config = Config
    { _maxSend  :: Int
    , _maxRecv  :: Int
    , _minPeers :: Int
    , _maxPeers :: Int
    , _myCtxt   :: Context
    } deriving Show

-- Information about a specific peer.
-- Three categories of data:
--      Immutable and persistant (persistant when exists even when peer dies)
--      Mutable and persistant
--      Mutable and temporary
data Peer = Peer
    { _addr   :: Addr
    , _hist   :: TVar Hist
    , _pear   :: TMVar Pear
    } deriving Show

-- Peers should be unique with respect to address
instance Eq Peer where
    a == b = addr a == addr b

data Hist = Hist
    { _up   :: Integer
    , _down :: Integer
    } deriving Show

data Pear = Pear
    { _context :: Context
    , _status  :: Status
    , _has     :: PieceMap
    , _lastMsg :: UnixTime
    , _toQ     :: Queue Message
    , _fromQ   :: Queue Message
    } deriving Show

data Status = Status
    { _choked      :: Bool
    , _choking     :: Bool
    , _interesting :: Bool
    , _interested  :: Bool
    } deriving Show

data Queue a = Queue [a] [a]

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue read write) = Queue read (x:write)

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue q@(Queue []     []) = (Nothing, q)
dequeue   (Queue []     ys) = let x:xs = reverse ys in (x, Queue xs [])
dequeue   (Queue (x:xs) ys) = (x, Queue xs ys)

-- Basically TChan with no repeats and with 'remove'
newtype TShan a = TShan (TVar [a]) deriving Show

newTShan :: STM (TShan a)
newTShan = TShan <$> newTVar []

readTShan :: Eq a => TShan a -> STM a
readTShan (TShan tvar) = do
    list' <- readTVar tvar
    case list' of
        (x:xs) -> do
            writeTVar tvar xs
            return x
        _ -> retry

-- Returns whether anything was added
writeTShan :: Eq a => TShan a -> a -> STM Bool
writeTShan = (. ins) . modifyTShan

-- Returns whether anything was removed
rmvTShan :: Eq a => TShan a -> a -> STM Bool
rmvTShan = (. rmv) . modifyTShan

-- Helper
modifyTShan :: TShan a -> ([a] -> (Bool, [a])) -> STM Bool
modifyTShan (TShan tvar) f = do
    list' <- readTVar tvar
    let (bool, list) = f list'
    writeTVar tvar list
    return bool

ins x l@(y:ys) = if x == y then (False, l) else clump y $ ins x ys
ins x [] = (True, [x])

rmv x (y:ys) = if x == y then (False, ys) else clump y $ rmv x ys
rmv x [] = (True, [])

clump x (y, xs) = (y, x:xs)

----------------------------------------
-- CETERA
----------------------------------------

