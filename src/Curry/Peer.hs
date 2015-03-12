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
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Lazy
import           Control.Monad
import           Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import           Data.Maybe
import           Data.UnixTime
import           Network.Info
import qualified Network.Simple.TCP as T
import           Network.Socket hiding (send)
import           Network.Socket.ByteString (send)
import           Network.Socket.ByteString.Lazy (getContents)
import           Prelude hiding (getContents)
import           System.Timeout

maybeAdd :: Env -> Addr -> STM (Maybe Peer)
maybeAdd env theirAddr = do
    peers' <- readTVar $ peers env
    if elem theirAddr . map addr $ peers'
     then return Nothing
     else do
        newGuy <- newPeer theirAddr
        modifyTVar (peers env) (newGuy :)
        return $ Just newGuy

-- Listen on a virtual port
react :: Env -> Peer -> IO ()
react env peer = forever $ do
    msg <- atomically . readTChan $ from peer
    time <- getUnixTime
    atomically . modifyTMVar (status peer) $ \s -> s { lastMsg = time }
    print msg
    -- case msg of
    --     Keepalive -> return ()
    --     Choke -> return ()
    --     Unchoke -> return ()
    --     Interested -> return ()
    --     Bored -> return ()
    --     Have ix -> return ()
    --     Bitfield m -> return ()
    --     Request ix off len -> return ()
    --     Piece ix off bytes -> return ()
    --     Cancel ix off len -> return ()

-- Serve on out port
beThere :: Env -> IO ()
beThere env = T.serve T.HostAny (addrPort $ whoami env) $ \(sock, sockAddr) ->
    atomically (maybeAdd env $ unSockAddr sockAddr)
     >>= maybe (return ()) (flip (play env) sock)

-- Connect to peer object
meet :: Env -> Peer -> IO ()
meet env peer = T.connect (addrIp $ addr peer)
                          (addrPort $ addr peer)
                          (play env peer . fst)

safeSend :: Socket -> B.ByteString -> IO ()
safeSend sock bytes = do
    sent <- send sock bytes
    when (sent /= B.length bytes) . throw $ Noitpecxe "not all sent"

-- The middleman between the peer and its corresponding socket
play :: Env -> Peer -> Socket -> IO ()
play env p@Peer{..} sock = do

        -- Handshake
        safeSend sock . mkShake $ ourShake env

        -- Lazily get their stream
        input <- getContents sock

        print $ L.take 10 input

        case parse parseShake input of
            Fail _ _ str -> throwIO $ Noitpecxe str
            Done t r -> when (r == ourShake env) $ do

                -- Initial peer status
                itime <- getUnixTime
                let startStatus = Status M.empty True True False False itime (someCtxt r)

                -- Initialize peer status
                atomically $ putTMVar status startStatus
                
                forkIO $ react env p

                catch (void . concurrently mouth $ ears t)
                    . (const :: IO () -> SomeException -> IO ())
                    . void
                    . atomically
                    $ takeTMVar status
  where

    -- Listen to outbound channel and send.
    -- Will make more descriptive exceptions soon.
    mouth = forever $ do
        msg <- atomically $ readTChan to
        let bytes = mkMsg msg
            len = B.length bytes
        sent <- send sock $ mkInt (toInteger len) `B.append` bytes
        when (sent /= 4 + len) . throw $ Noitpecxe "couldn't send stuff"

    -- Listen on socket and react.
    -- Parent will catch pattern match fail in fromJust, so it's safe.
    ears = evalStateT $ forever $ getMsg >>= (liftIO . atomically . writeTChan from)

getMsg :: StateT L.ByteString IO Message
getMsg = do
    len <- StateT $ \st -> case parse parseInt st of
        Fail _ _ str -> throwIO $ Noitpecxe str
        Done t r -> return (r, t)
    bytes <- StateT $ return . L.splitAt (fromInteger len)
    case parse parseMsg bytes of
        Fail _ _ str -> liftIO . throwIO $ Noitpecxe str
        Done t r ->
            if L.null t
            then liftIO . throwIO $ Noitpecxe "had leftover"
            else return r

----------------------------------------
-- HELPERS
----------------------------------------

-- NOW USING LAZY BYTESTRING
-- -- Receive an 'a' via tcp (throws exception on failed parse)
-- -- State type is leftover
-- recv' :: Socket -> Integer -> Parser a -> StateT B.ByteString IO a
-- recv' sock max parser = case gets (parse parser) of
--     Fail _ _ str -> liftIO . throw $ Noitpecxe str
--     Partial f -> f
--   where
--     getSome :: Integer -> (B.ByteString -> IResult B.ByteString r) -> (r, B.ByteString)
--     getSome left f = do
--         let less = min left 4096

ourShake :: Env -> Handshake
ourShake env = Handshake "BitTorrent protocol" (myCtxt $ config env) (ourId env) (infoHash $ metaInfo env)

newPeer :: Addr -> STM Peer
newPeer theirAddr = Peer theirAddr <$> newTVar (Hist 0 0)
                                   <*> newEmptyTMVar
                                   <*> newTChan
                                   <*> newTChan

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
