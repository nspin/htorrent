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
                       , xport      :: Word
                       , uploaded   :: Word
                       , downloaded :: Word
                       , left       :: Word
                       , event      :: Maybe THP_Event
                       }

data THP_Event = Started | Stopped | Completed


contact :: B.ByteString -> THP_Req -> IO (Maybe String)
contact base thp = case req of
    Nothing -> return Nothing
    Just req -> do
        resp <- simpleHTTP req
        return $ case resp of
            Left _ -> Nothing
            Right (Response {rspBody = body}) -> Just body
  where
    uri = B.unpack base ++ "?" ++ urlEncodeVars (queries thp)
    req = do
        theuri <- parseURI uri
        return Request { rqURI     = theuri
                       , rqMethod  = GET
                       , rqHeaders = []
                       , rqBody    = []
                       }


shw Started   = "started"
shw Stopped   = "stopped"
shw Completed = "completed"

queries :: THP_Req -> [(String, String)]
queries THP_Req { info_hash  = info_hashV
                , peer_id    = peer_idV
                , xport      = portV
                , uploaded   = uploadedV
                , downloaded = downloadedV
                , left       = leftV
                , event      = eventV
                }
        = [ ("info_hash"  , from160 info_hashV)
          , ("peer_id"    , from160 peer_idV)
          , ("port"       , show portV)
          , ("uploaded"   , show uploadedV)
          , ("downloaded" , show downloadedV)
          , ("left"       , show leftV)
          ] ++ case eventV
               of   Just e  -> [("event", shw e)]
                    Nothing -> []

-- Hoping that show escapes for us
from160 :: Word160 -> String
from160 (Word160 a b c d e) = show
                            . B2.pack
                            . map fromIntegral
                            $ concatMap chunkify [a, b, c, d, e]

chunkify :: Word32 -> [Word32]
chunkify x = [ shiftR x 24
             , shiftR x 16
             , shiftR x  8
             , x
             ]

-- -- There's gotta be a better way to do this...
-- from160 :: Word160 -> String
-- from160 (Word160 a b c d e) = mapM escape
--                             $ map (chr. fromIntegral)
--                             $ concatMap chunkify [a, b, c, d, e]

-- escape :: Char -> String
-- escape c = if or $ map ($ c) [isAsciiUpper, isAsciiLower, isDigit, (`elem` ".,
