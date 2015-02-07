module STest where

import Network
import System.IO

main = do
    sock <- listenOn $ PortNumber 6881
    print True
    (handle, name, number) <- accept sock
    print handle
    print name
    print number
    line <- hGetLine handle
    print line
