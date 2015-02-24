module Curtis.Tracker.THP
    ( mkURL
    ) where

import           Curtis.Types
import           Curtis.Parsers.Bencode
import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.Word
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import           Data.Attoparsec.ByteString
import           Control.Lens

mkURL :: (GP, GT) ->  -> String
mkURL Traq { tracker    = tracker'
           , info_hash0 = info_hash'
           , peer_id    = peer_id'
           , pport      = pport'
           , status0    = OurStatus { uploaded0   = uploaded'
                                    , downloaded0 = downloaded'
                                    , left0      = left'
                                    }
           , event      = event'
           , ip         = ip'
           , key        = key'
           , trackerid  = trackerid'
           }
  = tracker' ++ "?" ++ intercalate "&"
        ( maybe "" (("event=" ++ ) . encodeEvent) event'
        : [ "info_hash="  ++ urifyBS info_hash'
          , "peer_id="    ++ urifyBS peer_id'
          , "port="       ++ show pport'
          , "uploaded="   ++ show uploaded'
          , "downloaded=" ++ show downloaded'
          , "left="       ++ show left'
          , "ip="         ++ ip'
          , "key="        ++ urifyBS key'
          , "trackerid="  ++ trackerid'
          ]
        )
        
encodeEvent :: OurEvent -> String
encodeEvent Started   = "started"
encodeEvent Stopped   = "stopped"
encodeEvent Completed = "completed"

-- getTResp :: BValue -> Maybe TResponse
-- getTResp ben = do
--     dict <- getDict ben
--     case lookup "failure reason" dict of
--          Just (BString reason) -> Nothing -- Just (Left $ C.unpack reason)
--          Nothing     -> do
--             interval'   <- lookup "interval"   dict >>= getInt
--             complete'   <- lookup "complete"   dict >>= getInt
--             incomplete' <- lookup "incomplete" dict >>= getInt
--             peerStuff   <- lookup "peers"      dict
--             peers'      <- parseUncompressedPeers peerStuff `mplus` parseCompressedPeers peerStuff
--             Just TResponse
--                 { warning_response = fmap C.unpack (lookup "warning_response" dict >>= getString)
--                 , interval = interval'
--                 , min_interval = lookup "min_interval" dict >>= getInt
--                 , tracker_id = fmap C.unpack (lookup "min_interval" dict >>= getString)
--                 , complete = complete'
--                 , incomplete = incomplete'
--                 , peers = peers'
--                 }

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

urifyBS :: B.ByteString -> String
urifyBS = concatMap urify8 . B.unpack

urify8 :: Word8 -> String
urify8 byte = ['%', toHexHalf $ shiftR byte 4, toHexHalf $ byte .&. 15]

toHexHalf :: Word8 -> Char
toHexHalf = genericIndex "0123456789ABCDEF"
