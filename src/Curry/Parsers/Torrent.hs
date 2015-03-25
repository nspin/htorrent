{-# LANGUAGE TemplateHaskell, RecordWildCards, FlexibleInstances #-}

module Curry.Parsers.Torrent
    (

    , Torrent(..)
    , infoHash
    , source
    , funfo
    , infoDict

    , Source(..)
    , _Announce
    , _Announces
    , _Nodes

    , Info(..)
    , private
    , pieces
    , pieceLen
    , fileDesc

    , Files(..)
    , _One
    , _Many

    , File(..)
    , name
    , len
    , md5sum

    , Funfo(..)
    , comment
    , creator
    , birthday

    --

    , readTorrent

    ) where

import           Curry.Common
import           Curry.Parsers.Bencode

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.List
import           Data.Maybe

----------------------------------------
-- TYPES
----------------------------------------

data Torrent = Torrent
    { _infoHash :: Wird169
    , _source   :: Source
    , _funfo    :: MetaMeta
    , _infoDict :: Info
    } deriving Show

data Source = Announce String
            | Announces [[String]]
            | Nodes [LameAddr] --> 
            deriving Show

data Info = Info
    { _private  :: Bool
    , _pieces   :: [Word160]
    , _pieceLen :: Integer
    , _fileDesc :: Files
    } deriving Show

data Files = One { file :: (File String) }
           | Many { root :: String, files :: [File [String]] }
           deriving Show

data File a = File
    { _name   :: a
    , _len    :: Integer
    , _md5sum :: Maybe Word128
    } deriving Show

data Funfo = Funfo
    { _comment  :: Maybe String
    , _creator  :: Maybe String
    , _birthday :: Maybe String
    } deriving Show

----------------------------------------
-- READERS
----------------------------------------

readTorrent :: B.ByteString -> Maybe Torrent
readTorrent bytes = do
    dict <- mkReader parseBDict bytes

----------------------------------------
-- GETTERS
----------------------------------------

getMeta :: B.ByteString -> Either String Meta
getMeta bytes = MetaInfo <$> announce `mplus` announces `mplus` nodes
                         <*> hashify bytes
                         <*>
                         <*> MetaMeta (aux "comment")
                                      (aux "createdBy")
                                      (aux "creationDate")
  where
    dict = getBValue bytes >>= getDict
    aux str = eitherToMaybe . fmap C.unpack $ leekup str dict >>= getString

getTorrent :: BValue -> Either String Torrent
getTorrent = getDict >=> \dict -> do

    announce <- leekup "announce" dict >>= getString

    info <- leekup "info" dict >>= getInfo

    let

    return $ Torrent
        (C.unpack announce)
        (eitherToMaybe $ leekup "announce-list" dict >>= getList
            >>= mapM (getList >=> mapM (fmap C.unpack . getString)))
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

    fileDesc <- one <+> many

    return $ Info
        (maybe False (== 1) (eitherToMaybe $ leekup "private" info >>= getInt))
        pieceLen
        pieces
        fileDesc

----------------------------------------
-- TEMPLATE HASKELL
----------------------------------------

makeLenses ''Torrent
makePrisms ''Source
makeLenses ''Info
makePrisms ''Files
makeLenses ''File
makeLenses ''Funfo
