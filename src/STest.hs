module STest where

import Network.Simple.TCP

main :: IO ()
main = serve (Host "127.0.0.1") "6881" $ \(sock, addr) -> do
    print sock
    print addr
