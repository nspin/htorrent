{-# LANGUAGE RecordWildCards, FlexibleInstances #-}

module Curry.Peer where

import           Curry.Common
import           Curry.Types
import           Curry.Parsers.PWP
import           Curry.Parsers.Torrent

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import           Data.Maybe
import           Data.UnixTime
import           Network.Info
import qualified Network.Simple.TCP as T
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString
import           System.Timeout

newPeer :: Addr -> STM Peer
newPeer theirAddr = Peer theirAddr <$> newTVar startHist <*> newEmptyTMVar <*> newTChan

maybeAdd :: Env -> Addr -> STM (Maybe Peer)
maybeAdd env theirAddr = do
    present <- fmap (elem theirAddr . map addr) . readTVar $ peers env
    if present
     then return Nothing
     else do
        newGuy <- newPeer theirAddr
        modifyTVar (peers env) (newGuy :)
        return $ Just newGuy

-- Serve on out port
beThere :: Env -> IO ()
beThere env = T.serve T.HostAny (addrPort $ whoami env) $ \(sock, sockAddr) ->
    atomically (maybeAdd env $ unSockAddr sockAddr) >>= maybe (return ()) (meet env sock)

peerBatch :: Env -> [Addr] -> IO ()
peerBatch env = atomically . mapM (maybeAdd env) >=> mapM_ f . ((:[]) . head) . catMaybes
  where
    f :: Peer -> IO ()
    f peer = void . forkIO $ connect' peer (flip (meet env) peer)

connect' :: Peer -> (Socket -> IO r) -> IO r
connect' peer = T.connect (addrIp $ addr peer) (addrPort $ addr peer) . (. fst)

meet :: Env -> Socket -> Peer -> IO ()
meet env sock p@Peer{..} = catch go
                         . (const :: IO () -> SomeException -> IO ())
                         . void
                         . atomically
                         $ takeTMVar status
  where

    go = do

        -- Handshake
        sent <- send sock $ ourShake env
        head <- recv sock 20 -- protocol
        exts <- recv sock 8  -- reserved bytes
        hash <- recv sock 20 -- infohash
        them <- recv sock 20 -- their id

        print head
        print exts
        print hash
        print them

        when (sent == 68 && head == ourHead && hash == infoHash (metaInfo env)) $ do

            itime <- getUnixTime
            ctxt <- extract getCtxt exts

            -- Initialize peer status
            atomically . putTMVar status
                       . startStatus itime
                       $ merge (myCtxt $ config env) ctxt

            void $ concurrently mouth ears

    -- Listen to outbound channel and send.
    -- Will make more descriptive exceptions soon.
    mouth = forever $ do
        msg <- atomically $ readTChan mailbox
        let bytes = mkMsg msg
            len = B.length bytes
        sent <- send sock $ mkBigEnd (toInteger len) `B.append` bytes
        when (sent /= 4 + len) . throw $ PatternMatchFail "couldn't send stuff"

    -- Listen on socket and react.
    -- Parent will catch pattern match fail in fromJust, so it's safe.
    ears = forever $ do
        len' <- recv sock 4
        print len'
        len <- extract getBigEnd len'
        msg' <- recv sock (fromInteger len)
        print msg'
        print $ B.length msg'
        print $ getMsg msg'
        msg <- extract getMsg msg'
        time <- getUnixTime
        atomically . modifyTMVar status $ \s -> s { lastMsg = time }
        case msg of
            Keepalive -> putStrLn "Keepalive"
            Choke -> putStrLn "Choke"
            Unchoke -> putStrLn "Unchoke"
            Interested -> putStrLn "Interested"
            Bored -> putStrLn "Bored"
            Have ix -> putStrLn "Have"
            Bitfield m -> putStrLn "Bitfield"
            Request ix off len -> putStrLn "Request"
            Piece ix off bytes -> putStrLn "Piece"
            Cancel ix off len -> putStrLn "Cancel"

----------------------------------------
-- UTILS
----------------------------------------

-- May never use (but rather have a 'last heard from' field in peer status
timeout_ :: Int -> IO () -> IO ()
timeout_ limit f = do
    result <- timeout limit f
    case result of
        Nothing -> throw $ PatternMatchFail "timeout"
        Just () -> return ()

unSockAddr :: SockAddr -> Addr
unSockAddr (SockAddrInet pnum haddr) = Addr (show pnum) . show $ IPv4 haddr
unSockAddr (SockAddrInet6 pnum _ haddr6 _) = Addr "wat" "wat"
unSockAddr (SockAddrUnix _) = Addr "wat" "wat"

----------------------------------------
-- SOME IMPORTANT VALUES
----------------------------------------

ourHead :: B.ByteString
ourHead = B.singleton 19 `B.append` C.pack "BitTorrent protocol"

ourShake :: Env -> B.ByteString
ourShake env = ourHead `B.append` B.replicate 8 0
                       `B.append` infoHash (metaInfo env)
                       `B.append` ourId env

startHist :: Hist
startHist = Hist 0 0

startStatus :: UnixTime -> Context -> Status
startStatus = Status M.empty True True False False
