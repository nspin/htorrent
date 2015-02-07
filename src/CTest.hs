module CTest where

import Network
import System.IO

main = do
    (handle, name, number) <- connectTo "137.22.184.35" $ PortNumber 6881
    print handle
    print name
    print number
    hPutStrLn "hello world"
