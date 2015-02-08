{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable, RecordWildCards #-}

module Curry.State where

import           Curry.Parsers.PWP

import           Control.Concurrent.Chan.ReadOnly
import           Control.Concurrent.MVar.ReadOnly
import           Control.Concurrent.Chan.WriteOnly
import           Control.Concurrent.MVar.WriteOnly
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Data
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Data.SafeCopy
import           Data.Typeable
import           Data.Word
import           Network.Socket
import           System.IO

----------------------------------------
-- VARIOUS SORTS OF STATE
----------------------------------------

-- State common to all torrents for this specific instance of the client
data Global = Global
    { port     :: Integer -- should this be specific to dl?
    , pid      :: B.ByteString
    , key      :: B.ByteString
    , minPeers :: Integer
    , maxPeers :: Integer
    } deriving Show

------------------------------------------------

-- [To]rrent te[mp]orary state and environment

-- TODO: env only has >> peerCan :: Chan Peer

data Tomp = Tomp
    { peers     :: [Peer]
    , chunkNum  :: Integer
    , chunkProg :: M.Map Chunk B.ByteString
    } deriving Show

-- Information about a specific peer. Always exists in an MVar
data Peer = Peer
    { channel  :: MVar Message
    , mstuff   :: PeerMut
    } deriving Show

data PeerMut = PeerMut
    { up     :: ReadOnlyMVar Integer
    , has    :: ReadOnlyMVar (M.Map Integer Bool)
    , chunks :: ReadOnlyChan [(Chunk, B.ByteString)]
    , status :: MVar Status
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

------------------------------------------------

-- [Tor]rent [p]ersistent state.
data Torp = Torp
    { infoHash   :: B.ByteString
    , funfo      :: Funfo
    , fileDesc   :: FileDesc
    , size       :: Integer
    , downloaded :: Integer
    , uploaded   :: Integer
    , trackers   :: Trackers
    , pieceLen   :: Integer
    , pieceMap   :: M.Map Integer (Either B.ByteString  Handle)
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

data Funfo = Funfo
    { comment      :: Maybe String
    , createdBy    :: Maybe String
    , creationDate :: Maybe String
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

data Trackers = Trackers
    { announce     :: String
    , announceList :: [String]
    , private      :: Bool
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

data FileDesc = One (FileInfo String) | Many String [FileInfo [String]]
  deriving (Eq, Ord, Read, Show, Data, Typeable)
    
data FileInfo a = FileInfo
    { name   :: a
    , len    :: Integer
    , md5sum :: Maybe B.ByteString
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

----------------------------------------
-- CETERA
----------------------------------------

-- query for acid state
-- To allow types with MVars and Chans to allow show (which will only be
-- used for debugging)

instance Show (MVar a) where
    show _ = "(an mvar exists here)"

instance Show (Chan a) where
    show _ = "(an mvar exists here)"
