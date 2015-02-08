startStatus :: Status
startStatus = Status M.empty True True False False
module Curry.Peer where

import           Curry.Types

import qualified Data.ByteString as B
import           Network.Simple.TCP
import           Network.Socket.ByteString

-- Serve on out port
beThere :: Env -> IO ()
beThere env = serveFork (addrIp (whoami env)) (addrPort (whoami env)) $ (sock, addr) -> do
    let theirAddr = unSockAddr addr
    atmomically $ do
        alreadyThere <- 
    

peerBatch :: Env -> [Addr] -> IO ()
peerBatch = do
    

meet :: Env -> Peer -> Socket -> IO ()
meet env peer sock = catch go . atomically . takeTMVar $ status peer

  where

    go = do

        -- Handshake
        sent <- send sock $ ourShake env
        head <- recv sock 19 -- protocol
        exts <- recv sock 8  -- reserved bytes
        hash <- recv sock 20 -- infohash
        them <- recv sock 20 -- their id

        when (sent == 68 && head == ourHead && hash == infoHash (metaInfo env)) $ do

            -- Initialize peer status
            atomically . putTMVar (status peer)
                       . startStatus
                       . merge (context $ config env)
                       $ getCtxt exts

            concurrently mouth ears

        -- Listen to outbound channel and send.
        -- Will make more descriptive exceptions soon.
        mouth = forever $ do
            msg <- takeChan outChan
            let bytes = mkMsg msg
                len = B.length bytes
            sent <- send sock $ mkBigEnd len `append` bytes
            when (sent != 4 + len) . throw $ Exception "couldn't send stuff"

        -- Listen on socket and react.
        -- Parent will catch pattern match fail in fromJust, so it's safe.
        ears = forever $ do
            len <- recv sock 4   >>= check getBigEnd
            msg <- recv sock len >>= check getMsg
            case msg of
                Keepalive -> return ()
                Choke -> modifyMVar status $ \s -> s { choked = True }
                Unchoke -> modifyMVar status $ \s -> s { choked = False }
                Interested -> modifyMVar status $ \s -> s { interested = True }
                Bored -> modifyMVar status $ \s -> s { interested = False }
                Have ix -> modifyMVar status $ \s -> s { has = insert ix True $ has s }
                Bitfield m -> modifyMCtrl status $ \s -> { has = M.union m $ has s }
                Request ix off len -> putStrLn ("Peer requested chunk: " ++ show ix ++ ", " show off ++ ", " ++ show len)
                Piece ix off bytes -> putStrLn ("Got chunk: " ++ show ix ++ ", " show off)
                Cancel ix off len -> putStrLn ("Peer canceled chunk: " ++ show ix ++ ", " show off ++ ", " ++ show len)

----------------------------------------
-- UTILS
----------------------------------------

check :: Either String b -> IO b
check f x = case f x of
    (Left  str) -> throw $ SomeException str
    (Right val) -> return val

unSockAddr :: SockAddr -> Addr
unSockAddr (SockAddrInet pnum haddr) = Addr pnum haddr
unSockAddr (SockAddrInet6 pnum _ haddr6 _) = Addr pnum haddr6
unSockAddr (SockAddrUnix _) = Addr "wat" "wat"

----------------------------------------
-- SOME IMPORTANT VALUES
----------------------------------------

ourHead :: B.ByteString
ourHead = B.singleton 19 `B.append` C.pack "BitTorrent protocol"

ourShake :: Environment -> B.ByteString
ourShake env = ourHead `append` infoHash (metaInfo env) `append` ourId env

startStatus :: Context -> Status
startStatus = Status M.empty True True False False

startHist :: Hist
startHist = Hist 0 0
