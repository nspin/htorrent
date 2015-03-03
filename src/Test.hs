module Test where

import           Curry.Tracker
import           Curry.Environment
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
import           Control.Monad.Reader
import           Control.Monad.Trans.State.Lazy
import           Network.Simple.TCP
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import qualified Network.Wreq as W

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

    url <- evalStateT (runReaderT (mkURL Nothing) env) (CommSt Nothing 0 0)
    resp <- W.get url

    let Right thing = do
            let bytes = L.toStrict $ resp ^. W.responseBody
            rsp <- getBValue bytes >>= getResp
            return $ head (pears rsp)
        hs = B.concat [ourHead, B.pack $ replicate 8 0, infoHash meta, C.pack "thisisjustalittltest"]

    connect (pearIp thing) (show $ pearPort thing) $ \(s, _) -> do
        print "CONNECTED"
        send s hs
        print "SENT"
        print hs
        them <- recv s 128
        print them
