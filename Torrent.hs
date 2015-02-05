module Torrent ( Torrent(..)
               , InfoBucket(..)
               , FileInfo(..)
               , torrentize
               ) where

import           Bencode
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import           Control.Monad
import           Network.URL

data Torrent = Torrent { announce :: URL
                       , announceList :: Maybe [URL]
                       , comment :: Maybe B.ByteString
                       , createdBy :: Maybe B.ByteString
                       , createdDate :: Maybe B.ByteString
                       , info :: InfoBucket
                       }
               deriving Show

data InfoBucket = InfoBucket { fileStuff   :: Either FileInfo [(FileInfo, B.ByteString)]
                             , name        :: B.ByteString
                             , pieceLength :: Integer
                             , pieces      :: B.ByteString
                             }
                  deriving Show

data FileInfo = FileInfo { fileLength :: Integer
                         , md5sum     :: Maybe B.ByteString
                         }
                deriving Show

torrentize :: BValue -> Maybe Torrent

torrentize val = do

    dict        <- getDict val
    announceVal <- lookP dict "announce" >>= getString >>= (importURL . B.unpack)
    infoVal     <- lookP dict "info"     >>= getDict   >>= infoize

    return Torrent { announce     = announceVal
                   , announceList = lookP dict "announce-list" >>= getList >>= mapM (\x -> getString x >>= (importURL . B.unpack))
                   , comment      = lookP dict "comment"       >>= getString
                   , createdBy    = lookP dict "created by"    >>= getString
                   , createdDate  = lookP dict "creation date" >>= getString
                   , info         = infoVal
                   }

infoize :: M.Map B.ByteString BValue -> Maybe InfoBucket

infoize dict = do

    fileStuffVal   <- mplus one many
    nameVal        <- lookP dict "name"   >>= getString
    pieceLengthVal <- lookP dict "length" >>= getInt
    piecesVal      <- lookP dict "pieces" >>= getString

    return InfoBucket { fileStuff   = fileStuffVal
                      , name        = nameVal
                      , pieceLength = pieceLengthVal
                      , pieces      = piecesVal
                      }
  where

    one :: Maybe (Either FileInfo [(FileInfo, B.ByteString)])
    one = do
        lengthVal <- lookP dict "length" >>= getInt
        return $ Left FileInfo { fileLength = lengthVal
                               , md5sum = lookP dict "md5sum" >>= getString
                               }

    many :: Maybe (Either FileInfo [(FileInfo, B.ByteString)])
    many = liftM Right (lookP dict "files" >>= getList >>= mapM getPair)

    getPair :: BValue -> Maybe (FileInfo, B.ByteString)
    getPair file = do
        fileDict  <- getDict file
        lengthVal <- lookP fileDict "length" >>= getInt
        pathVal   <- lookP fileDict "path"   >>= getString
        return $ ( FileInfo { fileLength = lengthVal
                            , md5sum = lookP dict "md5sum" >>= getString
                            }
                 , pathVal
                 )

lookP :: (M.Map B.ByteString a) -> String -> Maybe a
lookP dict stringKey = M.lookup (B.pack stringKey) dict
