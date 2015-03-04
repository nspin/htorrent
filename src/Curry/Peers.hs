module Peers where

import           Curry.Common
import           Curry.Environment

import qualified Data.ByteString as B
import           Network.Simple.TCP
import           Network.Socket.ByteString

ourHead :: B.ByteString
ourHead = B.singleton 19 `B.append` ourProtocol

ourProtocol :: B.ByteString
ourProtocol = C.pack "BitTorrent protocol"

meet :: Environment -> Socket -> IO ()
meet env (sock, addr) = do
    sent <- send sock $ ourBody `B.append` pearId (ident env)
    when (sent == 68) $ do
        theirBody <- recv sock 48
        when (theirBody == ourBody) $ do
            theirId <- recv sock 20
            let theirIdent = unCurry Ident theirId theirAddr
                update :: STM (Maybe (IO (), IO ()))
                update = 
  where
    ourBody = ourHead `B.append` infoHash (metaInfo env)
    theirAddr = case addr of
        SockAddrInet pnum haddr -> (pnum, haddr)
        SockAddrInet6 pnum _ haddr6 _ -> (pnum, haddr6)
        SockAddrUnix _ -> ("wat", "wat")
