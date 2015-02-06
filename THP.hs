module THP ( THP_Req(..)
           , THP_Event(..)
           , contact
           ) where

import           Data.Bits
import           Data.Char
import           Data.Word
import           Data.Digest.SHA1

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as B2

import           Network.HTTP.Base
import           Network.HTTP
import           Network.URI

data THP_Req = THP_Req { info_hash  :: Word160
                       , peer_id    :: Word160
                       , rport      :: Word
                       , uploaded   :: Word
                       , downloaded :: Word
                       , left       :: Word
                       , event      :: Maybe THP_Event
                       }

data THP_Event = Started | Stopped | Completed

instance Show THP_Event where
    show Started   = "started"
    show Stopped   = "stopped"
    show Completed = "completed"

contact :: String -> THP_Req -> IO (Maybe String)
contact base thp = case req of
    Nothing -> return Nothing
    Just req -> do
        resp <- simpleHTTP req
        return $ case resp of
            Left _ -> Nothing
            Right (Response {rspBody = body}) -> Just body
  where
    uri = base ++ '?' : urlEncodeVars (queries thp)
    req = do
        theuri <- parseURI uri
        return Request { rqURI     = theuri
                       , rqMethod  = GET
                       , rqHeaders = []
                       , rqBody    = []
                       }

queries :: THP_Req -> [(String, String)]
queries THP_Req { info_hash  = info_hash'
                , peer_id    = peer_id'
                , rport      = port'
                , uploaded   = uploaded'
                , downloaded = downloaded'
                , left       = left'
                , event      = event'
                }
        = [ ("info_hash"  , from160 info_hash')
          , ("peer_id"    , from160 peer_id')
          , ("port"       , show port')
          , ("uploaded"   , show uploaded')
          , ("downloaded" , show downloaded')
          , ("left"       , show left')
          ] ++ case event'
               of   Just e  -> [("event", shw e)]
                    Nothing -> []

from160 :: Word160 -> String
from160 (Word160 a b c d e) = show
                            . B.pack
                            . map fromIntegral
                            $ concatMap chunkify [a, b, c, d, e]

chunkify :: Word32 -> [Word32]
chunkify x = [ shiftR x 24
             , shiftR x 16
             , shiftR x  8
             , x
             ]
