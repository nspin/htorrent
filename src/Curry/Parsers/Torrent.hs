{-# LANGUAGE FlexibleInstances #-}

module Curry.Parsers.Torrent where

import           Curry.Parsers.Bencode

import           Control.Monad
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.List
import           Data.Maybe

----------------------------------------
-- TYPES
----------------------------------------

-- All of the general meta-meta info and meta-info relevant to a torrent.
-- Size is in there for convenience (calculating left)

data MetaInfo = MetaInfo
    { infoHash :: B.ByteString
    , torrent  :: Torrent
    , size     :: Integer
    } deriving Show

-- Internal representation of a .torrent file

data Torrent = Torrent
    { announce     :: String
    , announceList :: Maybe [[String]]
    , comment      :: Maybe String
    , createdBy    :: Maybe String
    , creationDate :: Maybe String
    , info         :: Info
    } deriving Show

data Info = Info
    { private  :: Bool
    , pieceLen :: Integer
    , pieces   :: [B.ByteString]
    , fileDesc :: FileDesc
    } deriving Show

data FileDesc = One (FileInfo String) | Many String [FileInfo [String]]
  deriving Show

data FileInfo a = FileInfo
    { name   :: a
    , len    :: Integer
    , md5sum :: Maybe B.ByteString
    } deriving Show

----------------------------------------
-- GETTERS
----------------------------------------

getMeta :: B.ByteString -> Either String MetaInfo
getMeta bytes = MetaInfo <$> hashify bytes <*> (getBValue bytes >>= getTorrent)

getTorrent :: BValue -> Either String Torrent
getTorrent = getDict >=> \dict -> do

    announce <- leekup "announce" dict >>= getString

    info <- leekup "info" dict >>= getInfo

    let aux str = eitherToMaybe . fmap C.unpack $ leekup str dict >>= getString

    return $ Torrent
        (C.unpack announce)
        (eitherToMaybe $ leekup "announce-list" dict >>= getList
            >>= mapM (getList >=> mapM (fmap C.unpack . getString)))
        (aux "comment")
        (aux "createdBy")
        (aux "creationDate")
        info

getInfo :: BValue -> Either String Info
getInfo = getDict >=> \info -> do

    pieceLen <- leekup "piece length" info >>= getInt

    pieces <- leekup "pieces" info >>= getString
              >>= (eitherResult . (`feed` B.empty) . parse (many1 $ P.take 20))

    let one = fmap One $ FileInfo
                  <$> fmap C.unpack (leekup "name" info >>= getString)
                  <*> (leekup "length" info >>= getInt)
                  <*> return (eitherToMaybe (leekup "md5sum" info >>= getString))

        many = Many
            <$> fmap C.unpack (leekup "name" info >>= getString)
            <*> ( leekup "files" info
              >>= getList
              >>= mapM ( getDict >=> \file -> FileInfo
                      <$> (leekup "path" file >>= getList >>= mapM (fmap C.unpack . getString))
                      <*> (leekup "length" file >>= getInt)
                      <*> return (eitherToMaybe $ leekup "md5sum" file >>= getString)
                       )
                )

    fileDesc <- mplus one many

    return $ Info
        (maybe False (== 1) (eitherToMaybe $ leekup "private" info >>= getInt))
        pieceLen
        pieces
        fileDesc

----------------------------------------
-- AUX
----------------------------------------

instance MonadPlus (Either String) where
    mzero = Left "mzero"
    mplus r@(Right _) _  = r
    mplus _ r@(Right _) = r
    mplus _ l@(Left  _) = l

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left  _) = Nothing
