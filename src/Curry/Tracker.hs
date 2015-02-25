{-# LANGUAGE RecordWildCards #-}

module Curry.Tracker
    ( askTrack
    ) where

import           Curry.Types
import           Curry.Parsers.Bencode

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Bits
import           Data.Char
import           Data.Word
import           Data.List
import           Data.List.Split
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import           Data.Attoparsec.ByteString
import           Data.Maybe
import qualified Network.Wreq as W
import           Prelude hiding (GT)

-- Regularly gets updated info (and keepalives) from tracker,
-- spawing new processes for each new peer reported and adding
-- information about the processes to ST for the brain module.

askTrack :: Global -> Tomp -> AcidState Tacid -> IO ()
askTrack Global{..} Tomp{..} acid = forever $ do

    SP{..} <- query' acid AskSP

    -- Update uploaded from channel
    up <- getChanContents upChan >>= (update acid . AddUps)

    let url = announce (torrent $ metainfo) ++ "?" ++ intercalate "&"
              ( catMaybes [ fmap (("numwant="   ++) . show   ) numwant
                          , fmap (("trackerid=" ++) . urifyBS) trackerId
                          ]
             ++ [ "info_hash="  ++ urifyBS (info_hash metainfo)
                , "peer_id="    ++ urifyBS id
                , "port="       ++ show portM
                , "uploaded="   ++ show up
                , "downloaded=" ++ show downloaded
                , "left="       ++ show (total - downloaded)
                -- , "ip="         ++
                , "key="        ++ urifyBS key
                ]
              )
        downloaded = piece_length (info $ torrent metainfo) * M.size complete
            + maybe 0 (sum . map ((\(x, y) -> y - x) . fst) . M.toList) incomplete
        total = piece_length (info $ torrent metainfo) * length (pieces . info $ torrent metainfo)

    resp <- W.get url

    print resp

    threadDelay interval
    
-- parseUncompressedPeers :: BValue -> Maybe (Either [(B.ByteString, String, Integer)] [(String, Integer)])
-- parseUncompressedPeers = fmap Left . (getList >=> mapM (getDict >=> \d ->
--     do peer_id' <- lookup "peer id" d >>= getString
--        ip'      <- lookup "ip"      d >>= getString
--        port'    <- lookup "port"    d >>= getInt
--        return (peer_id', C.unpack ip', port')))

-- parseCompressedPeers :: BValue -> Maybe (Either [(B.ByteString, String, Integer)] [(String, Integer)])
-- parseCompressedPeers = fmap (Right . map aux . chunksOf 6 . B.unpack) . getString
--   where aux [a, b, c, d, e, f] = ( intercalate "." $ map show [a, b, c, d]
--                                  , fromIntegral e * 256 + fromIntegral f
--                                  )

urifyBS :: B.ByteString -> String
urifyBS = concatMap urify8 . B.unpack

urify8 :: Word8 -> String
urify8 byte = ['%', toHexHalf $ shiftR byte 4, toHexHalf $ byte .&. 15]

toHexHalf :: Word8 -> Char
toHexHalf = genericIndex "0123456789ABCDEF"