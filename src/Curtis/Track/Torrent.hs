module Curtis.Track.Torrent
    ( Torrent(..)
    , torrentize
    ) where

import           Curtis.Bencode

import           Control.Monad

import           Data.Word
import           Data.Word8
import           Data.Digest.SHA1
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

-- All of the information in a torren file that curtis is,
-- at this point, capable of caring about. Curtis will grow,
-- though, so this will expand at some point.
data Torrent = Torrent { announce  :: String
                       , pieceLen  :: Integer
                       , fileStuff :: Either Integer [Integer]
                       , pieces    :: C.ByteString
                       , infoHash  :: Word160
                       }
               deriving Show

torrentize :: B.ByteString -> Maybe Torrent
torrentize bytes = do

    dict      <- readBen bytes              >>= getDict
    announce' <- fmap C.unpack (bookup "announce" dict >>= getString)
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
