module Curry.Core where

import           Curry.State
import           Curry.Tracker

import           Control.Concurrent
import           Control.Concurrent.MVar
import qualified Data.Map as M
import           Data.Lens

-- meet :: PeerID -> P ()
-- meet di@(addr, prt) = do
--     (ihash, mid) <- asks infoT
--     peers <- readMVar mvar
--     unless ((==) id' . id `any` peers)
--      . connect addr prt
--      $ \(sock, _) -> do
--         forkIO $ send sock $ concat [handhead, ihash, mid]
--         forkIO $ do
--             resp <- recv sock 1337
--             case resp of
--                 Nothing -> return ()
--                 Just bytes -> do
--                     -- todo test things like infohash and protocol
--                     forkIO $ forever $ do
--                         msgs <- asks msgsM
--                         liftIO $ do
--                             com <- readMVar msgs
--                             doCom com
--                     forever $ liftIO $ do
--                         req <- recv sock 133337
--                         parseAndDoReq

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

