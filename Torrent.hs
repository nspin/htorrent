module Torrent ( Torrent(..)
               , torrentize
               ) where

import           Wing
import           Bencode

import           Control.Monad

import           Data.Word
import           Data.Word8
import           Data.Digest.SHA1

-- See below for why both are required (spoiler - it has to do
-- with the crypto library. May soon implement SHA1 in here)
import qualified Data.ByteString.Char8 as B
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

    dict       <- readBen bytes             >>= getDict
    announce'  <- lookP "announce"     dict >>= getString
    info       <- lookP "info"         dict >>= getDict
    pieces'    <- lookP "pieces"       info >>= getString
    pieceLen'  <- lookP "piece length" info >>= getInt
    infoHash'  <- liftM hash $ rawInfo bytes

    let one  = liftM Left  $ lookP "length" info >>= getInt
        many = liftM Right $ lookP "files"  info >>= getList
                 >>= mapM (getDict >=> lookP "length" >=> getInt)

    fileStuff' <- mplus one many

    return Torrent { announce  = announce'
                   , pieceLen  = pieceLen'
                   , fileStuff = fileStuff'
                   , pieces    = pieces'
                   , infoHash  = infoHash'
                   }

lookP = lookup . wap
