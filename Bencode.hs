module Bencode ( BData(..)
               , readBen
               , writeBen
               ) where

import           Control.Applicative
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString.Char8 as P

data BData = BDict (M.Map B.ByteString BData)
           | BList [BData]
           | BString B.ByteString
           | BInteger Integer

writeBen :: BData -> B.ByteString
writeBen BDict    dict  = surround 'd' $ concatMap (\(x, y) -> append (writeFromString x) (writeBen y)) $ toList dict
writeBen BList    list  = surround 'l' $ concatMap writeBen list
writeBen BString  bytes = writeFromString bytes
writeBen BInteger n     = surround 'i' $ pack $ show n

writeFromString = (append $ B.pack $ b.length bytes) . cons ':'

surround :: Char -> Char -> ByteString -> ByteString
surround = (`snoc` e) . cons

readBen :: B.ByteString -> Maybe BData
readBen = undefined

parseBData =  parseBDict
          <|> parseBList
          <|> parseBString
          <|> parseBInteger

parseBDict = fmap (BDict . M.fromList) $
    P.char 'l' *> P.many1 (liftA2 (,) parseBString' parseBData) <* P.char 'e'

parseBList = fmap BList $
    P.char 'l' *> P.many1 parseBData <* P.char 'e'

parseBString = BString <$> parseBString'

parseBString' = (P.decimal <* P.char ':') >>= P.take

parseBInteger = fmap BInteger $
    P.char 'i' *> P.decimal <* P.char 'e'
