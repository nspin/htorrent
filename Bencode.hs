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

readBen :: B.ByteString -> Maybe BData
readBen = undefined

writeBen :: BData -> B.ByteString
writeBen = undefined

parseBData =  parseBDict
          <|> parseBList
          <|> parseBString
          <|> parseBInteger

parseBDict = fmap (BDict . M.fromList) $
    P.char 'l' *> P.many1 (liftA2 (,) parseBString parseBData) <* P.char 'e'

parseBList = fmap BList $
    P.char 'l' *> P.many1 parseBData <* P.char 'e'

parseBString = do
    n <- P.decimal
    P.char ':'
    BString <$> P.take n

parseBInteger = fmap BInteger $
    P.char 'i' *> P.decimal <* P.char 'e'
