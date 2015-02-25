module Current.Torrent
    ( torpify
    ) where

import           Curry.State
import           Curry.Parsers.Bencode

import           Control.Monad
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Either
import           Data.Maybe
import qualified Data.Map as M
import           Data.Word
import           Data.Word8
import           Prelude hiding (length)

torpify :: B.ByteString -> Either String Torp
torpify bytes = do

    dict <- getBVal bytes >>= getDict
    info <- leekup "info" dict >>= getDict

    let trackers :: Either String Trackers
        trackers = Trackers
                <$> get "announce"
                <%> eitherEmpty ( fmap (map C.unpack)
                                        (leekup "announce-list" dict >>= getList >>= mapM getString)
                                 )
                <%> either (const False) (== 1) (leekup "private" info >>= getInt)
        
        get  key = fmap C.unpack (          leekup key dict >>= getString)
        getM key = fmap C.unpack (toMaybe $ leekup key dict >>= getString)

        funfo :: Funfo
        funfo = Funfo (getM "comment") (getM "created by") (getM "creation date")

        one = fmap One $ FileInfo
                      <$> fmap C.unpack (leekup "name" info >>= getString)
                      <*> (leekup "length" info >>= getInt)
                      <%> toMaybe (leekup "md5sum" info >>= getString)

        many = Many
            <$> fmap C.unpack (leekup "name" info >>= getString)
            <*> ( leekup "files" info
              >>= getList
              >>= mapM ( getDict >=> \file -> FileInfo
                      <$> (leekup "path" file >>= getList >>= mapM (fmap C.unpack . getString))
                      <*> (leekup "length" file >>= getInt)
                      <*> return (toMaybe $ leekup "md5sum" file >>= getString)
                       )
                )

        pieces = leekup "pieces" info >>= getString
                  >>= ( eitherResult
                      . (`feed` B.empty)
                      . parse (many1 $ P.take 20)
                      )

    Torp <$> hashify bytes
         <*> trackers
         <*> return funfo
         <*> mplus one many
         <*> fmap (M.fromList . zip [0..] . map Right) pieces
         <%> 0

-- The opposite of fmap
infixl 1 <%>
(<%>) :: Applicative f => f (a -> b) -> a -> f b
f <%> x = f <*> pure x

toMaybe :: Either a b -> Maybe b
toMaybe (Right x) = Just x
toMaybe (Left  _) = Nothing

eitherEmpty :: Either a [b] -> [b]
eitherEmpty (Right x) = x
eitherEmpty (Left  _) = []
