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

-- Some monad stacks:
type GReader = ReaderT (GP, GT)

----------------------------------------
-- VARIOUS SORTS OF STATE
----------------------------------------

-- Each is named according to/parameterized over
-- the following qualifiers:
--      G = global (common to all instances of the client)
--      S = specific (specific to a certain torrent)
--      P = persistant (using acid state)
--      T = temporary (dies with instance of client)

-- Not yet implemented. Will contain config of some sort.
data GP = GP
    { numwant :: Maybe Int
    , portM   :: Int
    }

data GT = GT
    { id  :: B.ByteString
    , key :: B.ByteString
    } deriving Show

-- Up exists here (not just as the sum of ups in peers)
-- because peer mvars will be dropped when connection is lost.
data ST = ST
    { interval  :: Int
    , trackerId :: Maybe B.ByteString
    , upChan    :: Chan Int
    , peers     :: MVar [MVar Peer]
    } deriving Show

-- This one's acidic. As I learn more about acid state, queries and
-- updates will become more lens-like
data SP = SP
    { metainfo   :: MetaInfo
    , complete   :: M.Map Int        B.ByteString
    , incomplete :: Maybe (M.Map (Int, Int) B.ByteString)
    , uploaded   :: Int
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

------------------------------------------------

-- Information about a specific peer. Always exists in an MVar
data Peer = Peer
    { addr     :: String
    , portP    :: String
    , sock     :: Socket -- necessary?
    , relation :: Relation
    , status   :: (M.Map Int Bool)
    , up       :: Int
    , down     :: Int
    , instr    :: MVar Command
    } deriving Show

data Relation = Relation
    { choked      :: Bool
    , choking     :: Bool
    , inerested   :: Bool
    , interesting :: Bool
    } deriving Show

-- TODO
data Command = Command deriving Show

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
-- PEER WIRE PROTOCOL
----------------------------------------

data Handshake = Handshake String B.ByteString B.ByteString

data Message = Keepalive
             | Choke
             | Unchoke
             | Intersted
             | Bored
             | Have Int
             | Bitfield B.ByteString
             | Request Int Int Int
             | Piece Int Int B.ByteString
             | Cancel Int Int Int
             deriving Show

----------------------------------------
-- METAINFO
----------------------------------------

-- A bencoded value.
-- Note that bdict keys are strings, not bytestrings (or, if you prefer,
-- that bstrings are bytestrings). These are different because bstrings
-- may be binary data, whereas (in all implementations I know of) keys
-- are always text. Storing keys as strings allows string literals to
-- be used in lookup (rather than packing for each lookup, which is not
-- very efficient).
data BValue = BString B.ByteString
            | BInt Int
            | BList [BValue]
            | BDict [(String, BValue)]
            deriving (Eq, Show, Data, Typeable)

-- All of the information in(/about) a torrent file that curtis is,
-- at this point, capable of caring about.
-- Note the use to snake_case. This just mirrors the structure of the
-- dictionary within a torrent file.

data MetaInfo = MetaInfo
    { torrent   :: Torrent
    , info_hash :: B.ByteString
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

data Torrent = Torrent
    { announce      :: String
    , announce_list :: Maybe [String]
    , comment       :: Maybe String
    , created_by    :: Maybe String
    , creation_date :: Maybe Int
    , encoding      :: Maybe String
    , info          :: Info
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

data Info = Info
    { piece_length :: Int
    , pieces       :: [B.ByteString]
    , private      :: Bool
    , fileStuff    :: Either (FileInfo String) (String, [FileInfo [String]])
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

data FileInfo a = FileInfo
    { name   :: a
    , len    :: Int
    , md5sum :: Maybe B.ByteString
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

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
