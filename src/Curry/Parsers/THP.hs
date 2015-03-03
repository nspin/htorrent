module Curry.Parsers.THP
    ( THPresp(..)
    , Pear(..)
    , getResp
    ) where

import           Curry.Common
import           Curry.Parsers.Bencode

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
    { pears      :: [Pear]
    , complete   :: Integer
    , incomplete :: Integer
    , interval   :: Integer
    , minterval  :: Maybe Integer
    , warning    :: Maybe String
    , yourId     :: Maybe B.ByteString
    } deriving Show

data Pear = Pear
    { pearIp   :: String
    , pearPort :: Integer
    , pearId   :: Maybe B.ByteString
    } deriving Show

instance Eq Pear where
    x == y = pearIp x == pearIp y && pearPort x == pearPort y

getResp :: BValue -> Either String THPresp
getResp = getDict >=> \dict -> case leekup "failure reason" dict of
    Right bval -> fmap C.unpack (getString bval) >>= Left
    Left _ -> THPresp
           <$> (do prs <- leekup "peers" dict
                   uncompressedPeers prs <+> compressedPeers prs)
           <*> (leekup "complete" dict >>= getInt)
           <*> (leekup "incomplete" dict >>= getInt)
           <*> (leekup "interval" dict >>= getInt)
           <%> eitherToMaybe (leekup "min interval" dict >>= getInt)
           <%> C.unpack <$> eitherToMaybe (leekup "warning" dict >>= getString)
           <%> eitherToMaybe (leekup "tracker id" dict >>= getString)

uncompressedPeers :: BValue -> Either String [Pear]
uncompressedPeers = getList >=> mapM (getDict >=> \dict ->
    Pear <$> fmap C.unpack (leekup "ip" dict >>= getString)
         <*> (leekup "port" dict >>= getInt)
         <%> eitherToMaybe (leekup "peer id" dict >>= getString))

compressedPeers :: BValue -> Either String [Pear]
compressedPeers = fmap (map aux . chunksOf 6 . B.unpack) . getString
  where aux [a, b, c, d, e, f] = Pear (intercalate "." $ map show [a, b, c, d])
                                      (fromIntegral e * 256 + fromIntegral f)
                                      Nothing
