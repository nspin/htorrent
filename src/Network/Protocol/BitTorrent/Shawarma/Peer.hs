{-# LANGUAGE RecordWildCards, FlexibleInstances #-}

module Network.Protocol.BitTorrent.Shawarma.Peer where

import           Network.Protocol.BitTorrent.Shawarma.Common
import           Network.Protocol.BitTorrent.Shawarma.Types
import           Network.Protocol.BitTorrent.Shawarma.Parsers.PWP
import           Network.Protocol.BitTorrent.Shawarma.Parsers.Torrent

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Lazy
import           Control.Monad
import           Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import           Data.Maybe
import           Data.UnixTime
import           Network.Info
import qualified Network.Simple.TCP as T
import           Network.Socket hiding (send, recv)
import           Network.Socket.ByteString (send, recv)
import           Prelude hiding (getContents)
import           System.Timeout

maybeAdd :: Env -> Addr -> STM (Maybe Peer)
maybeAdd env theirAddr = do
    peers' <- readTVar $ peers env
    if elem theirAddr $ map addr peers'
     then return Nothing
     else do
        newGuy <- newPeer theirAddr
        modifyTVar (peers env) (newGuy :)
        return $ Just newGuy

-- Listen on a virtual port
react :: Env -> Peer -> IO ()
react env peer = do
    msg <- atomically . readTChan $ from peer
    time <- getUnixTime
    atomically $ status peer >=~ \s -> do
        modifyTMVar (status peer) $ \s -> s { lastMsg = time }
        writeTChan (sayChan env) $ show msg
        case msg of
            Keepalive  -> return ()
            Choke      -> modifyTMVar s $ setChoked True
            Unchoke    -> modifyTMVar s $ setChoked False
            Interested -> modifyTMVar s $ setInteresting True
            Bored      -> modifyTMVar s $ setInteresting False
            Have ix    -> modifyTMVar s $ opHas (M.insert ix True)
            Bitfield m -> modifyTMVar s $ opHas (M.union m)
            Request c  -> writeTShan (takes env) (c, peer)
            Piece c    -> writeTShan (gives env) (c, peer)
            Cancel c   -> rmvShan    (takes env) (c, peer)
  where
    s = status peer

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

-- The middleman between the peer and its corresponding socket
play :: Env -> Peer -> Socket -> IO ()
play env p@Peer{..} sock = do

        -- Handshake
        safeSend sock . mkShake $ ourShake env
        (theirShake, rest) <- runStateT (recv' sock parseShake) B.empty

        when (theirShake == ourShake env) $ do

                -- Initial peer status
                itime <- getUnixTime

                atmomically $ do
                    
                    to <- newTChan
                    from <- newTChan

                    let startPear = Pear M.empty
                                         startStatus
                                         itime
                                         (someCtxt theirShake)
                                         to
                                         from

                    putTMVar pear startPear
                    curr <- readTVar $ progress env
                    writeTChan to $ Bitfield curr

                let go = void $ traverse Concurrently
                              [ forever mouth
                              , void $ runStateT (forever ears) rest
                              , forever $ react env p
                              ]

                catch (void go)
                    . (const :: IO () -> SomeException -> IO ())
                    . void
                    . atomically
                    $ takeTMVar status
  where

    -- Listen to outbound channel and send.
    -- Will make more descriptive exceptions soon.
    mouth = (atomically $ readTChan to) >>= (safeSend sock . mkMsg)

    -- Listen on socket and react.
    -- Parent will catch pattern match fail in fromJust, so it's safe.
    ears = recv' sock parseMsg >>= (liftIO . atomically . writeTChan from)

----------------------------------------
-- HELPERS
----------------------------------------

(>=~) :: TMVar a -> (a -> STM ()) -> STM ()
m >=~ f = do
    val <- tryTakeMVar m
    case val of
        Nothing -> return ()
        Just x -> f x

safeSend :: Socket -> B.ByteString -> IO ()
safeSend sock bytes = do
    sent <- send sock bytes
    unless (sent == B.length bytes) . throw $ Noitpecxe "not all sent"

recv' :: Socket -> Parser r -> StateT B.ByteString IO r
recv' sock parser = aux $ parse parser
  where
    aux :: (B.ByteString -> Result r) -> StateT B.ByteString IO r
    aux f = do
        result <- gets f
        case result of
            Fail _ _ str -> throw $ Noitpecxe "HERE"
            Partial f' -> getSome >> aux f'
            Done t x -> put t >> return x
    getSome = liftIO (recv sock 4096) >>=  put

ourShake :: Env -> Handshake
ourShake env = Handshake "BitTorrent protocol" (myCtxt $ config env) (infoHash $ metaInfo env) (ourId env)

newPeer :: Addr -> STM Peer
newPeer theirAddr = Peer theirAddr <$> newTVar (Hist 0 0) <*> newEmptyTMVar

newStatus :: Status
newStatus = Status False False False False

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
