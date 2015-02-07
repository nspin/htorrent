module CTest where

import Network.Simple.TCP

main = connect "137.22.184.35" "6881" $ \(sock, addr) -> do
    print sock
    print addr
