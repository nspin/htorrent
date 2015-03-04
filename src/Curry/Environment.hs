module Curry.Environment where

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
data Peer = Peer
    { addr   :: Addr 
    , out    :: TChan Message
    , hist   :: TVar Hist
    , status :: TMVar Status
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
