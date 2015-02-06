module Curtis.Track.Torrent
    ( MetaInfo(..)
    , Torrent(..)
    , InfoStuff(..)
    , OneFile(..)
    , ManyFiles(..)
    , ManyFile(..)
    , getMeta
    ) where

import           Curtis.Bencode
import           Curtis.Internal

import           Control.Monad
import           Control.Applicative

import           Data.Word
import           Data.Word8
import           Data.Maybe
import           Data.Digest.SHA1
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Attoparsec.ByteString.Char8

data MetaInfo = MetaInfo Torrent Word160 deriving Show

-- All of the information in a torren file that curtis is,
-- at this point, capable of caring about. Curtis will grow,
-- though, so this will expand at some point.
data Torrent = Torrent
    { announce      :: String
    , announce_list :: Maybe [String]
    , comment       :: Maybe String
    , created_by    :: Maybe String
    , creation_date :: Maybe Word
    , infoStuff     :: InfoStuff
    }
  deriving Show

data InfoStuff = InfoStuff
    { piece_length :: Integer
    , pieces       :: [Word160]
    , private      :: Bool
    , fileStuff    :: Either OneFile ManyFiles
    }
  deriving Show

data OneFile = OneFile
    { nameO   :: String
    , lengthO :: Integer
    , md5sumO :: Maybe B.ByteString -- (Word64, Word64)
    }
  deriving Show

data ManyFiles = ManyFiles
    { nameM  :: String -- semanticaly different from OneFile name, so seperate.
    , filesM :: [ManyFile]
    }
  deriving Show

data ManyFile = ManyFile
    { pathM  :: [String]
    , lengthM :: Integer
    , md5sumM :: Maybe B.ByteString -- (Word64, Word64)
    }
  deriving Show

getMeta :: B.ByteString -> Maybe MetaInfo
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
            >>= get160s
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
