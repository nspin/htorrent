module Test where

import           Curry.Tracker
import           Curry.State
import           Curry.Parsers.Bencode
import           Curry.Parsers.PWP
import           Curry.Parsers.THP
import           Curry.Parsers.Torrent

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import           Control.Concurrent.STM
import           Control.Lens
import           Network.Simple.TCP
import           Network.Socket.ByteString as S
import           Network.Wreq

test name = do
    file <- B.readFile name
    a <- newTVarIO M.empty
    b <- newTVarIO M.empty
    c <- newTVarIO []
    let Right meta = getMeta file
        env = Environment
                (Config 30 55)
                (Ident 6881 (C.pack "thisisjustalittltest") (C.pack "thisismytestkeyaight"))
                meta
                a
                b
                c
    url <- atomically $ mkURL env (CommSt Nothing 0 0) Nothing
    resp <- get url
    let Right thing = do
            let bytes = L.toStrict $ resp ^. responseBody
            rsp <- getBValue bytes >>= getResp 
            return $ head (pears rsp)
        hs = mkShake $ Handshake ourProtocol (C.pack "thisisjustalittltest") (infoHash meta)
    connect (pearIp thing) (show $ pearPort thing) $ \(s, _) -> do
        print "CONNECTED"
        S.sendAll s hs
        print "SENT"
        print hs
        them <- S.recv s 4096
        print them
