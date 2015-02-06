module Torrent ( Torrent(..)
               , torrentize
               ) where

import           Wing
import           Bencode

import           Control.Monad

import           Data.Word
import           Data.Word8
import           Data.Digest.SHA1
import qualified Data.ByteString as B2

data Torrent = Torrent { announce  :: Wing
                       , pieceLen  :: Int
                       , fileStuff :: Either Int [Int]
                       , pieces    :: Wing
                       , infoHash  :: Word160
                       }
               deriving Show

torrentize :: B.ByteString -> Maybe Torrent
torrentize bytes = do

    dict      <- readBen bytes              >>= getDict
    announce' <- lookup "announce"     dict >>= getString
    info      <- lookup "info"         dict >>= getDict
    pieces'   <- lookup "pieces"       info >>= getString
    pieceLen' <- lookup "piece length" info >>= getInt
    infoHash' <- liftM hash $ rawInfo bytes

    let one  = liftM Left  $ lookup "length" info >>= getInt
        many = liftM Right $ lookup "files"  info >>= getList
                 >>= mapM (getDict >=> lookup "length" >=> getInt)

    fileStuff' <- mplus one many

    return Torrent { announce  = announce'
                   , pieceLen  = pieceLen'
                   , fileStuff = fileStuff'
                   , pieces    = pieces'
                   , infoHash  = infoHash'
                   }

-- Getters for parsing de-bencoded torrent files

getString :: BValue -> Maybe String
getString (BString v) = Just v
getString _ = Nothing

getInt :: BValue -> Maybe Int
getInt (BInt v) = Just v
getInt _ = Nothing

getList :: BValue -> Maybe [BValue]
getList (BList v) = Just v
getList _ = Nothing

getDict :: BValue -> Maybe [(String, BValue)]
getDict (BDict v) = Just v
getDict _ = Nothing
