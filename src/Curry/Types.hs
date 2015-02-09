module Curry.Types
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

import           Curry.Common
import           Curry.Parsers.PWP
import           Curry.Parsers.THP
import           Curry.Parsers.Torrent

import           Control.Applicative
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import qualified Data.Map as M
import           Data.UnixTime
import           Network.Socket

----------------------------------------
-- GENERAL STATE TYPES
----------------------------------------

-- Overall download environment
data Env = Env
    { config    :: Config
    , whoami    :: Addr
    , ourId     :: B.ByteString
    , ourKey    :: B.ByteString
    , metaInfo  :: MetaInfo
    , peers     :: TVar [Peer]
    , progress  :: TVar PieceMap
    , gives     :: TShan ((Chunk B.ByteString), Peer)
    , takes     :: TShan ((Chunk Integer     ), Peer)
    , sayChan   :: TChan String
    } deriving Show

-- General static settings
data Config = Config
    { maxSend  :: Int
    , maxRecv  :: Int
    , minPeers :: Int
    , maxPeers :: Int
    , myCtxt   :: Context
    } deriving Show

-- Information about a specific peer.
-- Three categories of data:
--      Immutable and persistant (persistant when exists even when peer dies)
--      Mutable and persistant
--      Mutable and temporary
data Peer = Peer
    { addr   :: Addr
    , hist   :: TVar Hist
    , pear   :: TMVar Pear
    } deriving Show

-- Peers should be unique with respect to address
instance Eq Peer where
    a == b = addr a == addr b

data Hist = Hist
    { up   :: Integer
    , down :: Integer
    } deriving Show

data Pear = Pear
    { has     :: PieceMap
    , status  :: Status
    , lastMsg :: UnixTime
    , context :: Context
    , to      :: S.Seq Message
    , from    :: S.Seq Message
    } deriving Show

data Status = Status
    { choked      :: Bool
    , choking     :: Bool
    , interesting :: Bool
    , interested  :: Bool
    } deriving Show

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
-- OPERATIONS
----------------------------------------

opHas          f s = s { has         = f (has s) }
setChoked      x s = s { choked      = x         }
setInteresting x s = s { interesting = x         }

addUp   n h = h { up   = (up   h) + n }
addDown n h = h { down = (down h) + n }
