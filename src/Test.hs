{-# LANGUAGE RecordWildCards #-}

module Test where

import           Curry.Common
import           Curry.Tracker
import           Curry.Types
import           Curry.Peer
import           Curry.Parsers.Bencode
import           Curry.Parsers.PWP
import           Curry.Parsers.THP
import           Curry.Parsers.Torrent

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import           Data.Maybe
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens hiding (Context)
import           Control.Monad.Reader
import           Control.Monad.Trans.State.Lazy
import           Network.Simple.TCP as T
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import qualified Network.Wreq as W

test name = do

    file <- B.readFile name
    a <- Pieces <$> newTVarIO M.empty <*> newTChanIO <*> newTVarIO undefined <*> newTChanIO <*> newTVarIO undefined
    b <- newTVarIO []

    let Right meta = getMeta file
        env = Env
                (Config 0 0 30 55 Context)
                (Addr undefined $ show 6881)
                (C.pack "anothertestidishere1")
                (C.pack "testkeytestkeytestke")
                meta
                a
                b

    print $ announce $ torrent meta

    url <- atomically $ mkURL env Nothing Nothing
    resp <- fmap (L.toStrict . (^. W.responseBody))
                 (W.get url)
                  >>= extract (getBValue >=> getResp)

    -- print resp

    -- let x@Addr{..} = pears resp !! 3

    -- print x

    -- T.connect addrIp addrPort $ \(sock, _) -> do
    --     print "YAY"
    --     let shk = mkShake $ ourShake env
    --     print shk
    --     s <- SB.send sock shk
    --     print s
    --     r <- SB.recv sock 20
    --     print r
    --     print "done"

    added <- atomically $ mapM (maybeAdd env) $ pears resp

    mapM_ (forkIO . meet env) $ catMaybes added

    forever $ threadDelay maxBound
