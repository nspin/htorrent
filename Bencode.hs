module Bencode ( BValue(..)
               , readBen
               , writeBen
               , getDict
               , getList
               , getString
               , getInt
               ) where

import           Control.Applicative
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as P

data BValue = BDict (M.Map B.ByteString BValue)
            | BList [BValue]
            | BString B.ByteString
            | BInt Integer
            deriving Show

getDict :: BValue -> Maybe (M.Map B.ByteString BValue)
getDict (BDict v) = Just v
getDict _ = Nothing

getList :: BValue -> Maybe [BValue]
getList (BList v) = Just v
getList _ = Nothing
             
getString :: BValue -> Maybe B.ByteString
getString (BString v) = Just v
getString _ = Nothing

getInt :: BValue -> Maybe Integer
getInt (BInt v) = Just v
getInt _ = Nothing

writeBen :: BValue -> B.ByteString
writeBen (BDict   dict ) = surround 'd' . B.concat . map writePair $ M.toList dict
writeBen (BList   list ) = surround 'l' . B.concat $ map writeBen list
writeBen (BString bytes) = writeBytes bytes
writeBen (BInt    n    ) = surround 'i' . B.pack $ show n

writePair :: (B.ByteString, BValue) -> B.ByteString
writePair (key, value) = B.append (writeBytes key) (writeBen value)

writeBytes :: B.ByteString -> B.ByteString
writeBytes bytes = B.append (B.pack . show $ B.length bytes) $ B.cons ':' bytes

surround :: Char -> B.ByteString -> B.ByteString
surround start = B.cons start . (`B.snoc` 'e')

readBen :: B.ByteString -> Maybe BValue
readBen bytes = case P.parseOnly parseBValue bytes
                of   Right bval -> Just bval
                     Left  _    -> Nothing

parseBValue =  parseBDict
           <|> parseBList
           <|> parseBString
           <|> parseBInt

parseBDict = fmap (BDict . M.fromList) $
    P.char 'd' *> P.many1 (liftA2 (,) parseBString' parseBValue) <* P.char 'e'

parseBList = do
    P.char 'l'
    things <- P.many1 parseBValue
    P.char 'e'
    return (BList things)
-- parseBList = fmap BList $
--     P.char 'l' *> P.many1 parseBValue <* P.char 'e'

parseBString = BString <$> parseBString'

parseBString' = (P.decimal <* P.char ':') >>= P.take

parseBInt = fmap BInt $
    P.char 'i' *> P.decimal <* P.char 'e'
