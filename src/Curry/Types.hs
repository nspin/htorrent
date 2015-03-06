module Curry.Types where

import           Curry.Common
import           Curry.Parsers.PWP
import           Curry.Parsers.THP
import           Curry.Parsers.Torrent

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.Map as M
import           Network.Socket
import           System.IO

----------------------------------------
-- OUR MONAD STACK (likely to change as
-- I decide to make concurrency
-- control more granular.)
----------------------------------------

-- type Curry = ReaderT Environment

----------------------------------------
-- COMMON TO THE ENTIRE INSANCE
----------------------------------------

data Environment = Environment
    { config    :: Config
    , whoami    :: Addr
    , ourId     :: B.ByteString
    , ourKey    :: B.ByteString
    , metaInfo  :: MetaInfo
    , currPiece :: TVar (M.Map Chunk B.ByteString)
    , pieceMap  :: TVar (M.Map Integer (Maybe  Handle))
    , peers     :: TVar [Peer]
    } deriving Show

data Config = Config
    { minPeers :: Integer
    , maxPeers :: Integer
    } deriving Show

data Addr = Addr
    { addrIp   :: String
    , addrPort :: String
    } deriving (Eq, Show)

-- Information about a part of a piece
data Chunk = Chunk
    { index :: Integer
    , start :: Integer
    , end   :: Integer
    } deriving (Show, Eq, Ord)

-- Information about a specific peer.
-- Perhaps mixing SMT and a normal MVar is
-- bad, but after as much thought as I'm
-- willing to give, I couldn't thing of an
-- actual reason.
data Peer = Peer
    { addr    :: Addr
    , hist    :: TVar Hist
    , inChan  :: Chan Message
    , outChan :: Chan Message
    , status  :: MVar Status
    } deriving Show

data Hist = Hist
    { up   :: Integer
    , down :: Integer
    } deriving Show

data Status = Status
    { has         :: (M.Map Integer Bool)
    , choked      :: Bool
    , choking     :: Bool
    , interesting :: Bool
    , interested  :: Bool
    } deriving Show
