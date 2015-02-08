module Curry.Types where

import           Curry.Common
import           Curry.Parsers.PWP
import           Curry.Parsers.THP
import           Curry.Parsers.Torrent

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.Map as M
import           Network.Socket

----------------------------------------
-- OUR MONAD STACK (likely to change as
-- I decide to make concurrency
-- control more granular.)
----------------------------------------

-- type Curry = ReaderT Environment

----------------------------------------
-- COMMON TO THE ENTIRE INSANCE
----------------------------------------

data Env = Env
    { config   :: Config
    , whoami   :: Addr
    , ourId    :: B.ByteString
    , ourKey   :: B.ByteString
    , metaInfo :: MetaInfo
    , pieces   :: TVar (M.Map Integer Piece)
    , peers    :: TVar [Peer]
    } deriving Show

data Config = Config
    { minPeers :: Integer
    , maxPeers :: Integer
    , myCtxt   :: Context
    } deriving Show

data Addr = Addr String String deriving Show

---------------------------------

-- Information about a specific peer.
data Peer = Peer
    { addr    :: Addr
    , hist    :: TVar Hist
    , status  :: TMVar Status
    , mailbox :: Chan Message
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
    , context     :: Context
    } deriving Show

---------------------------------------------

data Piece = Piece
    { md5sum   :: Maybe B.ByteString
    , sha1     :: B.ByteString
    , progress :: Progress
    , take     :: Chan (Chunk, B.ByteString -> IO (), IO ())
    , give     :: Chan (Integer, Integer, B.ByteString, Integer -> IO (), IO ())
    } deriving Show

data Progress = None | Some | All

-- Information about a part of a piece
data Chunk = Chunk
    { index :: Integer
    , start :: Integer
    , end   :: Integer
    } deriving (Show, Eq, Ord)
