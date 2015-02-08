module Curry.Parsers.THP
    ( getResp
    ) where

import Curry.Parsers.Bencode

data THPresp = THPresp
    {
    } deriving Show

getResp :: BValue -> Either String THPresp
getResp = getDict >=> \dict -> case leekup "failure reason" dict of
    Right bval -> bval >>= getString >>= \reason -> Left reason
    Left _ -> do
        fdsa

parseUncompressedPeers :: BValue -> Maybe (Either [(B.ByteString, String, Integer)] [(String, Integer)])
parseUncompressedPeers = fmap Left . (getList >=> mapM (getDict >=> \d ->
    do peer_id' <- lookup "peer id" d >>= getString
       ip'      <- lookup "ip"      d >>= getString
       port'    <- lookup "port"    d >>= getInt
       return (peer_id', C.unpack ip', port')))

parseCompressedPeers :: BValue -> Maybe (Either [(B.ByteString, String, Integer)] [(String, Integer)])
parseCompressedPeers = fmap (Right . map aux . chunksOf 6 . B.unpack) . getString
  where aux [a, b, c, d, e, f] = ( intercalate "." $ map show [a, b, c, d]
                                 , fromIntegral e * 256 + fromIntegral f
                                 )
