module Curtis.Core where

import Data.Map
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Lens

type T = ReaderT Info IO
type P = ReaderT Book IO

-- infohash then pid
data Info = (B.ByteString, B.ByteString)

data Book = Book { peerM :: MVar peer
                 , msgsM :: MVar (Seq Message)
                 , infoT :: Info
                 }

data Peer = Peer { id :: PeerID
                 , relation :: Relation
                 , status :: Map Integer Bool
                 , sock :: Socket
                 }

type PeerID = (String, String) -- addr, port

data Relation = Relation { choked      :: Bool
                         , choking     :: Bool
                         , inerested   :: Bool
                         , interesting :: Bool
                         }

pstr :: B.ByteString
pstr = C.pack "BitTorrent protocol"

handhead :: B.ByteString
handhead = concat [ B.pack 19
                  , pstr
                  , B.replicate 8 0
                  ]

meet :: PeerID -> P ()
meet di@(addr, prt) = do
    (ihash, mid) <- asks infoT
    peers <- readMVar mvar
    unless ((==) id' . id `any` peers)
     . connect addr prt
     $ \(sock, _) -> do
        forkIO $ send sock $ concat [handhead, ihash, mid]
        forkIO $ do
            resp <- recv sock 1337
            case resp of
                Nothing -> return ()
                Just bytes -> do
                    -- todo test things like infohash and protocol
                    forkIO $ forever $ do
                        msgs <- asks msgsM
                        liftIO $ do
                            com <- readMVar msgs
                            doCom com
                    forever $ liftIO $ do
                        req <- recv sock 133337
                        parseAndDoReq
