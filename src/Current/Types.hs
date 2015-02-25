{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}

module Current.Types where

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import qualified Data.ByteString as B
import           Data.Data
import qualified Data.Map as M
import           Data.SafeCopy
import           Data.Typeable
import           Data.Word
import           Network.Socket
import           Prelude hiding (GT)

----------------------------------------
-- VARIOUS SORTS OF STATE
----------------------------------------

-- State common to all torrents for this specific instance of the client
data Global = Global
    { portM   :: Int
    , numwant :: Maybe Int
    , id      :: B.ByteString
    , key     :: B.ByteString
    } deriving Show

-- [To]rrent te[mp]orary.
data Tomp = ST
    { interval  :: Int
    , trackerId :: Maybe B.ByteString
    , complete   :: M.Map Int        B.ByteString
    , incomplete :: Maybe (M.Map (Int, Int) B.ByteString)
    , peers     :: MVar [MVar Peer]
    , currPiece :: Map (Int, Int) B.ByteString
    } deriving Show

-- [T]orrent [acid] state.
data Tacid = Tacid
    { infoHash     :: B.ByteString
    , trackers     :: Trackers
    , funfo        :: Funfo
    , fileStuff    :: Either (FileInfo String) (String, [FileInfo [String]])
    , pieceMap     :: Map Int (Either B.ByteString B.ByteString)
    , uploaded     :: Int
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

------------------------------------------------

data Trackers = Trackers
    { announce     :: String
    , announceList :: [String]
    , private      :: Bool
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

data Funfo = Funfo
    { comment       :: Maybe String
    , createdBy    :: Maybe String
    , creationDate :: Maybe Int
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

data FileInfo a = FileInfo
    { name   :: a
    , len    :: Int
    , md5sum :: Maybe B.ByteString
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

-- Information about a specific peer. Always exists in an MVar
data Peer = Peer
    { peerId   :: (String, String)
    -- , sock     :: Socket -- necessary?
    , partMap  :: (M.Map Int Bool)
    , instr    :: Chan Message
    , status   :: MVar Relation
    , has      :: MVar (M.Map Int Bool)
    , up       :: MVar Int
    , down     :: MVar Int
    , parts    :: Chan ((Int, (Int, Int)), B.ByteString)
    } deriving Show

data Relation = Relation
    { choked      :: Bool
    , choking     :: Bool
    , inerested   :: Bool
    , interesting :: Bool
    } deriving Show

-- Acid stuff

askSP :: Query SP SP
askSP = ask

putSP :: SP -> Update SP ()
putSP = put

addUps :: [Int] -> Update SP Int
addUps ups = do
    st <- get
    let new = (uploaded st) + sum ups
    put (st { uploaded = new })
    return new

----------------------------------------
-- CETERA
----------------------------------------

-- To allow types with MVars and Chans to allow show (which will only be
-- used for debugging)

instance Show (MVar a) where
    show _ = "(an mvar exists here)"

instance Show (Chan a) where
    show _ = "(an mvar exists here)"

----------------------------------------
-- TEMPLATE HASKELL
----------------------------------------

$(deriveSafeCopy 0 'base ''SP)
$(deriveSafeCopy 0 'base ''MetaInfo)
$(deriveSafeCopy 0 'base ''Torrent)
$(deriveSafeCopy 0 'base ''Info)
$(deriveSafeCopy 0 'base ''FileInfo)

$(makeAcidic ''SP ['askSP, 'putSP, 'addUps])
