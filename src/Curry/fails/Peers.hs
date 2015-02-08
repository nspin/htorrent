module Peers where

import           Curry.Common
import           Curry.Environment

import qualified Data.ByteString as B
import           Network.Simple.TCP
import           Network.Socket.ByteString

pears <- atomically $ readTVar (peers environment)
when (not $ theirAddr `elem` map addr pears) $ do
  where

meet :: Environment -> Peer -> Socket -> IO ()
meet env peer sock = do
    sent <- send sock $ ourShake env
    theirBody <- recv sock 48
    recv sock 20
    when (sent == 68 && theirBody == ourBody env) $ do
        atomically $ putTMVar (status peer) startStatus
        forkIO $ catch mouth $ takeMVar status
        forkIO $ catch ears  $ takeMVar status
        forever $ do
            msg <- takeChan inChan
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
  where

    Peer{..} = peer

    mouth :: IO ()
    mouth = forever $ do
        msg <- takeChan outChan
        let bytes = mkMsg msg
            len = B.length bytes
        sent <- send sock $ mkBigEnd len `append` bytes
        when (sent != 4 + len) $ sClose sock

    ears :: IO ()
    ears = forever $ do
        lenBS <- recv sock 4
        case getBigEnd len of
            Left _ -> sClose sock
            Right len -> do
                msgBS <- recv sock len
                case getMsg msgBS of
                    Left _ -> sClose sock
                    Right msg -> putChan inChan msg

conDo :: IO Bool -> IO ()
conDo action = forever $ do
    readTMVar $ status peer
    success <- action
    when (not success) . void . atomically . takeTMVar $ status peer

communicate :: Environment -> Peer -> IO ()
communicate env peer = forever $ do
    readTMVar $ status peer
    len <- fmap getBigEnd $
    when (not success) . void . atomically . takeTMVar $ status peer

----------------------------------------
-- UTILS
----------------------------------------

unSockAddr :: SockAddr -> Addr
unSockAddr (SockAddrInet pnum haddr) = Addr pnum haddr
unSockAddr (SockAddrInet6 pnum _ haddr6 _) = Addr pnum haddr6
unSockAddr (SockAddrUnix _) = Addr "wat" "wat"

----------------------------------------
-- SOME IMPORTANT VALUES
----------------------------------------

ourProtocol :: B.ByteString
ourProtocol = C.pack "BitTorrent protocol"

ourHead :: B.ByteString
ourHead = B.singleton 19 `B.append` ourProtocol

ourBody = Environmnet -> B.ByteString
ourBody env = ourHead `B.append` infoHash (metaInfo env)

ourShake :: Environment -> B.ByteString
ourShake env = ourBody env `append` ourId env

startStatus :: Status
startStatus = Status M.empty True True False False

startHist :: Hist
startHist = Hist 0 0
