{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}

module Curry.State where

import           Curry.Parsers.PWP

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
    { port     :: Int -- should this be specific to dl?
    , id       :: B.ByteString
    , key      :: B.ByteString
    , minPeers :: Int
    , maxPeers :: Int
    } deriving Show

------------------------------------------------

-- [To]rrent te[mp]orary state and environment

-- TODO: env only has >> peerCan :: Chan Peer

data Tomp = Tomp
    { peers     :: [Peer]
    , chunkNum  :: Int
    , chunkProg :: M.Map Chunk B.ByteString
    } deriving Show

-- Information about a specific peer. Always exists in an MVar
data Peer = Peer
    { peerId   :: B.ByteString
    , channel  :: MVar Message
    , mstuff   :: PeerMut
    } deriving Show

data PeerMut = PeerMut
    { up     :: MVar Int
    , down   :: MVar Int
    , has    :: MVar (M.Map Int Bool)
    , chunks :: Chan [(Chunk, B.ByteString)]
    , status :: MVar Status
    } deriving Show

data Chunk = Chunk
    { index :: Int
    , start :: Int
    , end   :: Int
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
    { infoHash  :: B.ByteString
    , trackers  :: Trackers
    , funfo     :: Funfo
    , fileDesc  :: FileDesc
    , pieceLen  :: Int
    , pieceMap  :: M.Map Int (Either B.ByteString B.ByteString)
    , uploaded  :: Int
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

data Trackers = Trackers
    { announce     :: String
    , announceList :: [String]
    , private      :: Bool
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

data Funfo = Funfo
    { comment      :: Maybe String
    , createdBy    :: Maybe String
    , creationDate :: Maybe String
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

data FileDesc = One (FileInfo String) | Many String [FileInfo [String]]
  deriving (Eq, Ord, Read, Show, Data, Typeable)
    
data FileInfo a = FileInfo
    { name   :: a
    , len    :: Int
    , md5sum :: Maybe B.ByteString
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

-- Acid stuff

askTorp :: Query Torp Torp
askTorp = ask

putTorp :: Torp -> Update Torp ()
putTorp = put

mkURL :: Global -> Maybe B.ByteString -> Maybe Travent -> Query Torp String
mkURL GLobal{..} trackerID event = fmap urlify ask
  where urlify Torp{..} = announce (torrent $ metainfo) ++ "?" ++ intercalate "&"
              ( maybeToList $ fmap (("trackerid=" ++) . urifyBS) trackerId
             ++ [ "peer_id="    ++ urifyBS id
                , "port="       ++ show port
                , "numwant="    ++ show minPeers
                , "key="        ++ urifyBS key

                , "info_hash="  ++ urifyBS infoHash
                , "uploaded="   ++ show uploaded
                , "downloaded=" ++ show downloaded
                , "left="       ++ show (total - downloaded)
                ]
              )
          where
            downloaded = pieceLen * M.size (M.filter isRight pieceMap)
            total = pieceLen * M.size pieceMap

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

$(deriveSafeCopy 0 'base ''Torp)
$(deriveSafeCopy 0 'base ''Funfo)
$(deriveSafeCopy 0 'base ''Trackers)
$(deriveSafeCopy 0 'base ''FileDesc)
$(deriveSafeCopy 0 'base ''FileInfo)

$(makeAcidic ''Torp ['askTorp, 'putTorp, mkURL'])
