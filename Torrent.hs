module Torrent ( Torrent(..)
               , torrentize
               ) where

import           Bencode

import           Control.Monad

import           Data.Word
import           Data.Word8
import           Data.Digest.SHA1
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

data Torrent = Torrent { announce  :: C.ByteString
                       , pieceLen  :: Int
                       , fileStuff :: Either Int [Int]
                       , pieces    :: C.ByteString
                       , infoHash  :: Word160
                       }
               deriving Show

torrentize :: B.ByteString -> Maybe Torrent
torrentize bytes = do

    dict      <- readBen bytes              >>= getDict
    announce' <- bookup "announce"     dict >>= getString
    info      <- bookup "info"         dict >>= getDict
    pieces'   <- bookup "pieces"       info >>= getString
    pieceLen' <- bookup "piece length" info >>= getInt

    let one  = liftM Left  $ bookup "length" info >>= getInt
        many = liftM Right $ bookup "files"  info >>= getList
                 >>= mapM (getDict >=> bookup "length" >=> getInt)

    fileStuff' <- mplus one many
    infoHash'  <- hashify bytes

    return Torrent { announce  = announce'
                   , pieceLen  = pieceLen'
                   , fileStuff = fileStuff'
                   , pieces    = pieces'
                   , infoHash  = infoHash'
                   }


-- Getters for parsing de-bencoded torrent files

getString :: BValue -> Maybe C.ByteString
getString (BString v) = Just v
getString _ = Nothing

getInt :: BValue -> Maybe Int
getInt (BInt v) = Just v
getInt _ = Nothing

getList :: BValue -> Maybe [BValue]
getList (BList v) = Just v
getList _ = Nothing

getDict :: BValue -> Maybe [(C.ByteString, BValue)]
getDict (BDict v) = Just v
getDict _ = Nothing
