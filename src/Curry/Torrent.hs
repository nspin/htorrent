module Current.Torrent
    ( torpify
    ) where

import           Curtis.State
import           Curtis.Parsers.Bencode

import           Control.Monad
import           Control.Applicative
import           Data.Word
import           Data.Word8
import           Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Attoparsec.ByteString.Char8 as P
import           Prelude hiding (length)

torpify :: B.ByteString -> Maybe Torp
torpify bytes = do

    dict <- getDict bytes
    info <- lookup "info" dict >>= getInfo

    let trackers = Trackers
                <$> fmap C.unpack (lookup "announce" dict >>= getString)
                <*> fmap (map C.unpack) (lookup "announce-list" dict >>= getList >>= mapM getString)
                <*> maybe False (== 1) (lookup "private" info >>= getInt)

        funfo = Funfo
              <$> fmap C.unpack (lookup "comment" dict >>= getString)
              <*> fmap C.unpack (lookup "created by" dict >>= getString)
              <*> fmap C.unpack (lookup "creation date" dict >>= getString)

        one = (($ lookup "md5sum" dict >>= getString) . FileInfo)
             <$> lookup "name" dict >>= getString
             <*> lookup "length" dict >>= getInt

        many = Many
            <$> fmap C.unpack (lookup "name" dict >>= getString)
            <*> ( lookup "files" info
              >>= getList
              >>= mapM ( getDict >=> \file ->
                          (($ lookup "md5sum" file >>= getString) . FileInfo)
                      <$> lookup "path" file >>= getList >>= mapM getString
                      <*> lookup "length" file >>= getInt
                       )
                )

         pieces = lookup "pieces" dict >>= getString
                  >>= ( maybeResult
                      . (`feed` B.empty)
                      . parse (many1 $ P.take 20)
                      )

    return $ (Torp . ($ 0))
          <$> hashify bytes
          <*> trackers
          <*> funfo
          <*> mplus one many
          <*> (M.fromList . zip [0..] . map Right) pieces
