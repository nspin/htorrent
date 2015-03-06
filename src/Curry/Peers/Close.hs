module Curry.Peers.Close where

import           Curry.Parsers.PWP

import qualified Control.Concurrent
import qualified Control.Concurrent.Async

scanner :: Conext -> Message -> Either String Message
scanner conext msg = case msg of
    Keepalive     -> Right msg
    Choke         -> Right msg
    Unchoke       -> Right msg
    Interested    -> Right msg
    Bored         -> Right msg
    Have _        -> Right msg
    Bitfield _    -> Right msg
    Request _ _ _ -> Right msg
    Piece _ _ _   -> Right msg
    Cancel _ _ _  -> Right msg

mailman :: Peer -> Socket -> IO ()
mailman Peer{..} sock =
