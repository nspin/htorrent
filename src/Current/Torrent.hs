module Current.Torrent
    ( getMeta
    ) where

import           Curtis.Types
import           Curtis.Parsers.Bencode

import           Prelude hiding (length)

import           Control.Monad
import           Control.Applicative

import           Data.Word
import           Data.Word8
import           Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Attoparsec.ByteString.Char8 as P


getMeta :: B.ByteString -> Maybe MetaInfo
getMeta bytes = do
    torrent' <- getBVal bytes >>= getTorrent
    info_hash' <- hashify bytes 
    return MetaInfo { torrent = torrent'
                    , info_hash = info_hash'
                    }

getTorrent :: BValue -> Maybe Torrent
getTorrent = getDict >=> \dict -> do
    announce' <- lookup "announce" dict >>= getString
    info' <- lookup "info" dict >>= getInfo
    return Torrent { announce = C.unpack announce'
                   , announce_list = fmap (map C.unpack)
                                   $ lookup "announce-list" dict
                                 >>= getList
                                 >>= mapM getString
                   , comment = fmap C.unpack
                             $ lookup "comment" dict >>= getString
                   , created_by = fmap C.unpack
                                $ lookup "created by" dict >>= getString
                   , creation_date = lookup "creation date" dict >>= getString
                                 >>= (maybeResult . parse decimal)
                   , encoding = fmap C.unpack $ lookup "encoding" dict >>= getString
                   , info = info'
                   }

getInfo :: BValue -> Maybe Info
getInfo = getDict >=> \dict -> do
    piece_length' <- lookup "piece length" dict >>= getInt
    pieces' <- lookup "pieces" dict
           >>= getString
           >>= (maybeResult . (`feed` B.empty) . parse (many1 $ P.take 20))
    let one = fmap Left $ do
                name' <- lookup "name" dict >>= getString
                length' <- lookup "length" dict >>= getInt
                return FileInfo { name = C.unpack name'
                                , length = length'
                                , md5sum = lookup "md5sum" dict >>= getString
                                }
        many = fmap Right $ do
                 name' <- lookup "name" dict >>= getString
                 files' <- lookup "files" dict >>= getList >>= mapM (getDict >=> \file -> do
                    path' <- lookup "path" file >>= getList >>= mapM getString
                    length' <- lookup "length" file >>= getInt
                    return FileInfo { name = map C.unpack path'
                                    , length = length'
                                    , md5sum = lookup "md5sum" file >>= getString
                                    }
                    )
                 return (C.unpack name', files')
    fileStuff <- mplus one many
    return Info { piece_length = piece_length'
                , pieces       = pieces'
                , private      = fromMaybe False
                                 $ fmap (== 1)
                                 $ lookup "private" dict >>= getInt
                , fileStuff    = fileStuff
                }
