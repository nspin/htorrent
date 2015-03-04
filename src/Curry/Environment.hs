module Curry.Environment where

import           Curry.Common
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
    , whoami    :: (Ident, B.ByteString) -- (ident, key)
    , metaInfo  :: MetaInfo
    , currPiece :: TVar (M.Map Chunk B.ByteString)
    , pieceMap  :: TVar (M.Map Integer (Maybe  Handle))
    , peers     :: TVar [Peer]
    } deriving Show

data Config = Config
    { minPeers :: Integer
    , maxPeers :: Integer
    } deriving Show

data Ident = Ident
    { pearId   :: B.ByteString
    , pearIp   :: String
    , pearPort :: Integer
    } deriving (Eq, Show)

-- Information about a part of a piece
data Chunk = Chunk
    { index :: Integer
    , start :: Integer
    , end   :: Integer
    } deriving (Show, Eq, Ord)

-- Information about a specific peer.
data Peer = Peer
    { ident  :: Ident
    , mut    :: TVar MutPeer
    , toT    :: TChan Message
    , fromT  :: TChan Message
    } deriving Show

-- What a specific peer thread has
data MutPeer = MutPeer
    { has         :: (M.Map Integer Bool)
    , up          :: Integer
    , down        :: Integer
    , choked      :: Bool
    , choking     :: Bool
    , interesting :: Bool
    , interested  :: Bool
    } deriving Show
