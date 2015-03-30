{-# LANGUAGE RecordWildCards, TupleSections #-}

module Network.Protocol.BitTorrent.Shawarma.Tracker
    ( Travent(..)
    , mkURL
    , format
    ) where

import           Network.Protocol.BitTorrent.Shawarma.Common
import           Network.Protocol.BitTorrent.Shawarma.Types
import           Network.Protocol.BitTorrent.Shawarma.Parsers.Bencode
import           Network.Protocol.BitTorrent.Shawarma.Parsers.PWP
import           Network.Protocol.BitTorrent.Shawarma.Parsers.THP
import           Network.Protocol.BitTorrent.Shawarma.Parsers.Torrent

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.State.Lazy
import           Control.Monad.Trans
import           Data.Attoparsec.ByteString
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import           Data.Char
import           Data.List
import           Data.List.Split
import qualified Data.Map as M
import           Data.Maybe
import           Data.Word
import qualified Network.Wreq as W

-- data CommSt = CommSt
--     { trackerIdST   :: Maybe B.ByteString -- our tracker id
--     , intervalST    :: Integer -- from tracker
--     , minIntervelST :: Maybe Integer -- from tracker
--     } deriving Show

-- Event for tracker http requests
data Travent = Started | Stopped | Complete

instance Show Travent where
    show Started  = "started"
    show Stopped  = "stopped"
    show Complete = "complete"

-- -- Why now state of a state monad? Because is does not change enough.
-- track :: CommSt -> Tracking ()
-- track CommSt{..} = do
--     url <- ReaderT $ atomically . mkURL Nothing trackerIdST
--     wresp <- liftIO $ W.get url
--     let bytes = wresp ^. W.responseBody
--         rresp = getBValue (L.toStrict bytes) >>= getResp
--     case rresp of
--         Left issue -> liftIO $ putStrLn issue
--         Right resp -> do
--             let check :: ReaderT Environment STM
--             mapM (forkIO . meet) destinations

data URL = URL
    { base    :: String
    , queries :: [(String, String)]
    } deriving Show

format :: URL -> String
format URL{..} = base ++ "?" ++ intercalate "&" [ k ++ "=" ++ v | (k, v) <- queries ]

mkURL :: Env -> Maybe Travent -> Maybe B.ByteString -> STM URL
mkURL env event trackid = do

    peers' <- readTVar peers
    ups <- mapM (fmap up . readTVar . hist) peers'
    downs <- mapM (fmap down . readTVar . hist) peers'
    have <- (M.size . M.filter id) <$> readTVar progress

    return . URL (announce torrent) $
      ( catMaybes [ fmap (("trackerid",) . urifyBS) trackid
                  , fmap (("event"    ,) . show   ) event
                  ]
     ++ [("numwant"   ,  show minPeers                    )
        ,("port"      ,  show (addrPort whoami)           )
        ,("peer_id"   ,  urifyBS ourId                    )
        ,("key"       ,  urifyBS ourKey                   )
        ,("info_hash" ,  urifyBS infoHash                 )
        ,("uploaded"  ,  show (sum ups)                   )
        ,("downloaded",  show (sum downs)                 )
        ,("left"      ,  show (totalSize - toInteger have))
        ]
      )

  where
    Env{..} = env
    Config{..} = config
    MetaInfo{..} = metaInfo
    info' = info torrent
    totalSize = pieceLen info' * genericLength (pieces info')

urifyBS :: B.ByteString -> String
urifyBS = concatMap urify8 . B.unpack

urify8 :: Word8 -> String
urify8 byte = ['%', toHexHalf $ shiftR byte 4, toHexHalf $ byte .&. 15]

toHexHalf :: Word8 -> Char
toHexHalf = genericIndex "0123456789ABCDEF"
