module Peers where

import           Curry.Common
import           Curry.Environment

import qualified Data.ByteString as B
import           Network.Simple.TCP
import           Network.Socket.ByteString

ourProtocol :: B.ByteString
ourProtocol = C.pack "BitTorrent protocol"

ourHead :: B.ByteString
ourHead = B.singleton 19 `B.append` ourProtocol

ourBody = Environmnet -> B.ByteString
ourBody env = ourHead `B.append` infoHash (metaInfo env)

ourShake :: Environment -> B.ByteString
ourShake env = ourBody env `append` ourId env

pears <- atomically $ readTVar (peers environment)
when (not $ theirAddr `elem` map addr pears) $ do
  where
    theirAddr :: Addr
    theirAddr = case addr of
        SockAddrInet pnum haddr -> Addr pnum haddr
        SockAddrInet6 pnum _ haddr6 _ -> Addr pnum haddr6
        SockAddrUnix _ -> Addr "wat" "wat"


meet :: Environment -> Addr -> Socket -> IO ()
meet env peer sock = do
    sent <- send sock $ ourShake env
    theirBody <- recv sock 48
    recv sock 20
    when (sent == 68 && theirBody == ourBody env) $ do
        newPeer <- atomically $ Peer theirAddr <$> newTChan
                                               <*> newTVar startMut
                                               <*> newEmptyTMVar
        forkIO $ do
            msg <- readTChan $ out newPeer

startStatus = Status M.empty True True False False
startHist = Hist 0 0
