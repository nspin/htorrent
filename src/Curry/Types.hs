module Curry.Types
    ( Env(..)
    , Config(..)
    , Pieces(..)
    , Progress(..)
    , Peer(..)
    , Hist(..)
    , Status(..)
    , opHas
    , setChoked
    , setInteresting
    , done
    ) where

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
import           Data.UnixTime
import           Network.Socket

----------------------------------------
-- IMPORTANT ENVIRONMENT/STATE TYPES
----------------------------------------

data Env = Env
    { config    :: Config
    , whoami    :: Addr
    , ourId     :: B.ByteString
    , ourKey    :: B.ByteString
    , metaInfo  :: MetaInfo
    , ourPieces :: Pieces
    , peers     :: TVar [Peer]
    , output    :: Chan String
    } deriving Show

data Config = Config
    { maxSend  :: Int
    , maxRecv  :: Int
    , minPeers :: Int
    , maxPeers :: Int
    , myCtxt   :: Context
    } deriving Show

data Pieces = Pieces
    { progress  :: TVar (M.Map Integer Progress)
    , take      :: TChan      (Chunk Integer, B.ByteString -> IO (), IO ())
    , takeBin   :: TVar (MVar (Chunk Integer, B.ByteString -> IO (), IO ()))
    , give      :: TChan      (Chunk B.ByteString, Integer -> IO (), IO ())
    , giveBin   :: TVar (MVar (Chunk B.ByteString, Integer -> IO (), IO ()))
    } deriving Show

data Progress = None | Some | All deriving Show

---------------------------------

-- Information about a specific peer.
data Peer = Peer
    { addr   :: Addr
    , hist   :: TVar Hist
    , status :: TMVar Status
    , to     :: TChan Message
    , from   :: TChan Message
    } deriving Show

instance Eq Peer where
    a == b = addr a == addr b

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
    , lastMsg     :: UnixTime
    , context     :: Context
    } deriving Show

opHas f s = s { has = f (has s) }
setChoked x s = s { choked = x }
setInteresting x s = s { interesting = x }

----------------------------------------
-- UTILS
----------------------------------------

done :: Progress -> Bool
done All = True
done _ = False
