module Curtis.Tracker

import Curtis.Parsers.Bencode
import Control.Concurrent.MVar
import Control.Monad
import Network.Wreq

update :: Download -> MVar Progress ->  MVar [MVar Peer] -> IO ()
update dl mprog mmpeers = do
    prog <- readMVar mprog
    resp <- get $ mkURL TRequest { tracker = ttracker dl
                                 , info_hash = thash dl
                                 , peer_id = myid dl
                                 , pport = port dl
                                 , status = tstat prog
                                 , compact = False
                                 , no_peer_id = False
                                 , event = Nothing
                                 , ip = Nothing
                                 , numwant = Nothing
                                 , key = Just $ myid dl
                                 , trackerid = Nothing
                                 }
    
    = fmap ( ( getBVal
                  . L.toStrict
                  . (^. responseBody)
                  ) >=> getTResp
                )
         . get
         . mkURL
    mpeers <- readMVar mmpeers

