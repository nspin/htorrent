module Curtis.Core where

import Data.Map
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Lens

type T = ReaderT Download (StateT (MVar Tate) IO)


-- progress is a map from piece number to pairs of start-end bites of needed blocks
data Tate = Tate { progress :: Map Integer [(Integer, Integer)]
                 , --peers    :: [MVar Peer]
                 }

data Progress = Progress { pieces  :: Map Integer Bool
                         , current :: Integer
                         , cprog   :: [(Int, Int)]
                         , tstat   :: TStatus
                         }

-- infohash, pid, then pieces
data Download = Download { tracker  :: String
                         , myd      :: B.ByteString
                         , metahash :: B.ByteString
                         , myport   :: Integer
                         , phashes  :: [B.ByteString]
                         }

data Peer = Peer { id       :: PeerID -- necessary? (yes for tracker thread decisions)
                 , sock     :: Socket -- necessary?
                 , relation :: Relation
                 , status   :: (Map Integer Bool)
                 , up       :: Word
                 , down     :: Word
                 , cchan    :: Command
                 }

-- addr, port
type PeerID = (String, String)

data Relation = Relation { choked      :: Bool
                         , choking     :: Bool
                         , inerested   :: Bool
                         , interesting :: Bool
                         }

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

-- prints info for diagnostics
-- getTRespTEST :: TRequest -> IO (Maybe (Either String TResponse))
-- getTRespTEST req = do
--     print $ mkURL req
--     resp <- get $ mkURL req
--     print resp
--     let ben = L.toStrict (resp ^. responseBody)
--     print "\n\n"
--     print $ marse parseBen ben
--     print "\n\n"
--     return ((( marse parseBen
--                   . L.toStrict
--                   . (^. responseBody)
--            ) >=> parseTResp) resp)

