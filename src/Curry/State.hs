{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable, RecordWildCards #-}

module Curry.State where

import           Curry.Parsers.PWP

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
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
import           Prelude hiding (GT)

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
    { peerId   :: B.ByteString
    , channel  :: MVar Message
    , mstuff   :: PeerMut
    } deriving Show

data PeerMut = PeerMut
    { up     :: MVar Integer
    , down   :: MVar Integer
    , has    :: MVar (M.Map Integer Bool)
    , chunks :: Chan [(Chunk, B.ByteString)]
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
    , pieceMap   :: M.Map Integer Piece
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

data Piece = Hash B.ByteString | Data B.ByteString
  deriving (Eq, Ord, Read, Show, Data, Typeable)

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

mkURL :: Global -> Maybe B.ByteString -> Maybe Travent -> Query Torp String
mkURL Global{..} trackerId event = (`fmap` ask) $ \Torp{..} ->
    announce trackers ++ "?" ++ intercalate "&"
      ( catMaybes [ fmap (("trackerid=" ++) . urifyBS) trackerId
                  , fmap (("event="     ++) . show   ) event
                  ]
     ++ [ "peer_id="    ++ urifyBS pid
        , "port="       ++ show port
        , "numwant="    ++ show minPeers
        , "key="        ++ urifyBS key
        , "info_hash="  ++ urifyBS infoHash
        , "uploaded="   ++ show uploaded
        , "downloaded=" ++ show downloaded
        , "left="       ++ show (size - downloaded)
        ]
      )

urifyBS :: B.ByteString -> String
urifyBS = concatMap urify8 . B.unpack

urify8 :: Word8 -> String
urify8 byte = ['%', toHexHalf $ shiftR byte 4, toHexHalf $ byte .&. 15]

toHexHalf :: Word8 -> Char
toHexHalf = genericIndex "0123456789ABCDEF"

-- Event for tracker http requests

data Travent = Started | Stopped | Complete

instance Show Travent where
    show Started  = "started"
    show Stopped  = "stopped"
    show Complete = "complete"

-- To allow types with MVars and Chans to allow show (which will only be
-- used for debugging)

instance Show (MVar a) where
    show _ = "(an mvar exists here)"

instance Show (Chan a) where
    show _ = "(an mvar exists here)"

----------------------------------------
-- TEMPLATE HASKELL FOR ACID STATE
----------------------------------------

-- infoHash'   :: Query Torp B.ByteString
-- funfo'      :: Query Torp Funfo
-- fileDesc'   :: Query Torp FileDesc
-- size'       :: Query Torp Integer
-- downloaded' :: Query Torp Integer
-- uploaded'   :: Query Torp Integer
-- trackers'   :: Query Torp Trackers
-- pieceLen'   :: Query Torp Integer
-- pieceMap'   :: Query Torp (M.Map Integer Piece)

-- infoHash'   = ask infoHash   
-- funfo'      = ask funfo      
-- fileDesc'   = ask fileDesc   
-- size'       = ask size       
-- downloaded' = ask downloaded 
-- uploaded'   = ask uploaded   
-- trackers'   = ask trackers   
-- pieceLen'   = ask pieceLen   
-- pieceMap'   = ask pieceMap   

$(deriveSafeCopy 0 'base ''Torp)
$(deriveSafeCopy 0 'base ''Funfo)
$(deriveSafeCopy 0 'base ''Trackers)
$(deriveSafeCopy 0 'base ''FileDesc)
$(deriveSafeCopy 0 'base ''FileInfo)
$(deriveSafeCopy 0 'base ''Travent)
$(deriveSafeCopy 0 'base ''Global)
$(deriveSafeCopy 0 'base ''Piece)

$(makeAcidic ''Torp ['mkURL])
