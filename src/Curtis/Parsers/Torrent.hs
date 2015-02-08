module Curtis.Tracker.Torrent
    ( MetaInfo(..)
    , Torrent(..)
    , InfoStuff(..)
    , OneFile(..)
    , ManyFiles(..)
    , ManyFile(..)
    , getMeta
    ) where

import           Curtis.Bencode
import           Curtis.Common

import           Control.Monad
import           Control.Applicative

import           Data.Word
import           Data.Word8
import           Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Attoparsec.ByteString.Char8 as P


getMeta :: B.ByteString -> Either String MetaInfo
getMeta bytes = liftM2 MetaInfo (marse parseBen bytes >>= getTorrent)
                                (hashify bytes)

getTorrent :: BValue -> Maybe Torrent
getTorrent = getDict >=> \dict -> do
    announce' <- bookup "announce" dict >>= getString
    infoStuff' <- bookup "info" dict >>= getInfo
    return Torrent { announce = C.unpack announce'
                   , announce_list = fmap (map C.unpack)
                                   $ bookup "announce-list" dict
                                 >>= getList
                                 >>= mapM getString
                   , comment = fmap C.unpack
                             $ bookup "comment" dict >>= getString
                   , created_by = fmap C.unpack
                                $ bookup "created by" dict >>= getString
                   , creation_date = bookup "creation date" dict >>= getString
                                 >>= marse decimal
                   , infoStuff = infoStuff'
                   }

getInfo :: BValue -> Maybe InfoStuff
getInfo = getDict >=> \dict -> do
    piece_length' <- bookup "piece length" dict >>= getInt
    pieces' <- bookup "pieces" dict
          >>= getString
          >>= (maybeResult . (`feed` B.empty) . parse (many1 $ P.take 20))
    let one = fmap Left $ do
                name' <- bookup "name" dict >>= getString
                length' <- bookup "length" dict >>= getInt
                return OneFile { nameO = C.unpack name'
                               , lengthO = length'
                               , md5sumO = bookup "md5sum" dict >>= getString
                               }
        many = fmap Right $ do
                 name' <- bookup "name" dict >>= getString
                 files' <- bookup "files" dict >>= getList >>= mapM (getDict >=> \file -> do
                    path' <- bookup "path" file >>= getList >>= mapM getString
                    length' <- bookup "length" file >>= getInt
                    return ManyFile { pathM = map C.unpack path'
                                    , lengthM = length'
                                    , md5sumM = bookup "md5sum" file >>= getString
                                    }
                    )
                 return ManyFiles { nameM = C.unpack name', filesM = files' }
    fileStuff' <- mplus one many
    return InfoStuff { piece_length = piece_length'
                     , pieces       = pieces'
                     , private      = fromMaybe False
                                      $ fmap (== 1)
                                      $ bookup "private" dict >>= getInt
                     , fileStuff    = fileStuff'
                     }
