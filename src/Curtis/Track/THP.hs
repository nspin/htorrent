module Curtis.Track.THP
    ( THPrq(..)
    , TStatus(..)
    , TEvent(..)
    , TResponse(..)
    , getTHPResp
    ) where

import           Curtis.Bencode
import           Curtis.Internal
import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.Word
import           Data.Digest.SHA1
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import           Data.Attoparsec.ByteString
import           Network.Wreq
import           Control.Lens

getTHPResp :: THPrq -> IO (Maybe (Either String TResponse))
getTHPResp = fmap ( ( marse parseBen
                    . L.toStrict
                    . (^. responseBody)
                    ) >=> parseTResp
                  )
           . get
           . mkURL

data THPrq = THPrq { tracker    :: String
                   , info_hash  :: Word160
                   , peer_id    :: Word160
                   , pport      :: Word
                   , status     :: TStatus
                   , compact    :: Bool
                   , no_peer_id :: Bool
                   , event      :: Maybe TEvent
                   , ip         :: Maybe String
                   , numwant    :: Maybe Word
                   , key        :: Maybe Word160
                   , trackerid  :: Maybe String
                   }

data TStatus = TStatus { uploaded   :: Integer
                       , downloaded :: Integer
                       , left       :: Integer
                       }

data TEvent = Started | Stopped | Completed

mkURL :: THPrq -> String
mkURL THPrq { tracker    = tracker'
            , info_hash  = info_hash'
            , peer_id    = peer_id'
            , pport      = pport'
            , status     = TStatus { uploaded   = uploaded'
                                   , downloaded = downloaded'
                                   , left       = left'
                                   }
            , compact    = compact'
            , no_peer_id = no_peer_id'
            , event      = event'
            , ip         = ip'
            , numwant    = numwant'
            , key        = key'
            , trackerid  = trackerid'
            }
  = tracker' ++ "?" ++ intercalate "&"
        ( [ "info_hash="  ++ urify160 info_hash'
          , "peer_id="    ++ urify160 peer_id'
          , "port="       ++ show pport'
          , "uploaded="   ++ show uploaded'
          , "downloaded=" ++ show downloaded'
          , "left="       ++ show left'
          ]
       ++ if compact'    then ["compact=1"] else []
       ++ if no_peer_id' then ["no_peer_id=1"] else []
       ++ catMaybes [ fmap (("event=" ++ ) . encodeEvent) event'
                    , fmap ("ip="        ++) ip'
                    , fmap (("numwant="   ++) . show) numwant'
                    , fmap (("key="       ++) . urify160) key'
                    , fmap ("trackerid=" ++) trackerid'
                    ]
        )
        
encodeEvent :: TEvent -> String
encodeEvent Started   = "started"
encodeEvent Stopped   = "stopped"
encodeEvent Completed = "completed"

data TResponse = TResponse { warning_response :: Maybe String
                           , interval         :: Integer
                           , min_interval     :: Maybe Integer
                           , tracker_id       :: Maybe String
                           , complete         :: Integer
                           , incomplete       :: Integer
                           , peers            :: Either [(Word160, String, Integer)]
                                                        [(         String, Integer)]
                           }
                 deriving Show


parseTResp :: BValue -> Maybe (Either String TResponse)
parseTResp ben = do
    dict <- getDict ben
    case bookup "failure reason" dict of
         Just (BString reason) -> Just (Left $ C.unpack reason)
         Nothing     -> do
            interval'   <- bookup "interval"   dict >>= getInt
            complete'   <- bookup "complete"   dict >>= getInt
            incomplete' <- bookup "incomplete" dict >>= getInt
            peerStuff   <- bookup "peers"      dict
            peers'      <- parseUncompressedPeers peerStuff `mplus` parseCompressedPeers peerStuff
            return $ Right TResponse
                { warning_response = fmap C.unpack (bookup "warning_response" dict >>= getString)
                , interval = interval'
                , min_interval = bookup "min_interval" dict >>= getInt
                , tracker_id = fmap C.unpack (bookup "min_interval" dict >>= getString)
                , complete = complete'
                , incomplete = incomplete'
                , peers = peers'
                }

parseUncompressedPeers :: BValue -> Maybe (Either [(Word160, String, Integer)] [(String, Integer)])
parseUncompressedPeers = fmap Left . (getList >=> mapM (getDict >=> \d ->
    do peer_id' <- bookup "peer_id" d >>= getString >>= marse parse160
       ip'      <- bookup "ip"      d >>= getString
       port'    <- bookup "port"    d >>= getInt
       return (peer_id', C.unpack ip', port')))

parseCompressedPeers :: BValue -> Maybe (Either [(Word160, String, Integer)] [(String, Integer)])
parseCompressedPeers = fmap (Right . map aux . chunksOf 6 . B.unpack) . getString
  where aux [a, b, c, d, e, f] = ( intercalate "." $ map show [a, b, c, d]
                                 , shiftL 8 (fromIntegral e) .&. fromIntegral f
                                 )
