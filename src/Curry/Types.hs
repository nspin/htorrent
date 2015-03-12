module Curry.Types
    ( Env(..)
    , Config(..)
    , Peer(..)
    , Hist(..)
    , Status(..)
    , PieceNode(..)
    , Progress(..)
    , Chunk(..)
    , simplify
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
    , ourPieces :: TVar (M.Map Integer PieceNode)
    , peers     :: TVar [Peer]
    } deriving Show

data Config = Config
    { minPeers :: Integer
    , maxPeers :: Integer
    , myCtxt   :: Context
    } deriving Show

---------------------------------

-- Information about a specific peer.
data Peer = Peer
    { addr    :: Addr
    , hist    :: TVar Hist
    , status  :: TMVar Status
    , to      :: TChan Message
    , from    :: TChan Message
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
    , lastMsg     :: UnixTime
    , context     :: Context
    } deriving Show

---------------------------------------------

data PieceNode = PieceNode
    { md5sum   :: Maybe B.ByteString
    , sha1     :: B.ByteString
    , progress :: Progress
    , take     :: Chan (Chunk, B.ByteString -> IO (), IO ())
    , give     :: Chan (Integer, Integer, B.ByteString, Integer -> IO (), IO ())
    } deriving Show

data Progress = None | Some | All deriving Show

-- Information about a part of a piece
data Chunk = Chunk
    { index :: Integer
    , start :: Integer
    , end   :: Integer
    } deriving (Show, Eq, Ord)

----------------------------------------
-- UTILS
----------------------------------------

simplify :: M.Map Integer PieceNode -> M.Map Integer Bool
simplify = M.map (done . progress)

done :: Progress -> Bool
done All = True
done _ = False
