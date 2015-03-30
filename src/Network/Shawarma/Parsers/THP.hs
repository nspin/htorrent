module Network.Parsers.THP
    ( THPresp(..)
    , getResp
    ) where

import           Network.Shawarma.Common
import           Network.Shawarma.Parsers.Bencode

import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Control.Applicative
import           Control.Monad

data THPresp = THPresp
    { pears      :: [Addr]
    , complete   :: Integer
    , incomplete :: Integer
    , interval   :: Integer
    , minterval  :: Maybe Integer
    , warning    :: Maybe String
    , yourId     :: Maybe B.ByteString
    } deriving Show

getResp :: BValue -> Either String THPresp
getResp = getDict >=> \dict -> case leekup "failure reason" dict of
    Right bval -> fmap C.unpack (getString bval) >>= Left
    Left _ -> THPresp
           <$> fmap (map $ uncurry Addr)
                    (do prs <- leekup "peers" dict
                        uncompressedPeers prs <+> compressedPeers prs)
           <*> (leekup "complete" dict >>= getInt)
           <*> (leekup "incomplete" dict >>= getInt)
           <*> (leekup "interval" dict >>= getInt)
           <%> eitherToMaybe (leekup "min interval" dict >>= getInt)
           <%> C.unpack <$> eitherToMaybe (leekup "warning" dict >>= getString)
           <%> eitherToMaybe (leekup "tracker id" dict >>= getString)

uncompressedPeers :: BValue -> Either String [(String, String)]
uncompressedPeers = getList >=> mapM (getDict >=> \dict ->
    (,) <$> fmap C.unpack (leekup "ip" dict >>= getString)
        <*> fmap show (leekup "port" dict >>= getInt))

compressedPeers :: BValue -> Either String [(String, String)]
compressedPeers = fmap (map aux . chunksOf 6 . B.unpack) . getString
  where
    aux [a, b, c, d, e, f] =
        (intercalate "." $ map show [a, b, c, d], show $ fromIntegral e * 256 + fromIntegral f)
