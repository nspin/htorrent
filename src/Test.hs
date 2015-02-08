module Test where

import Curry.State
import Curry.Torrent
import Network.Wreq
import Data.ByteString as B
import Data.ByteString.Char8 as C
import Data.Acid

global :: Global
global = Global
    { port     = 6881
    , pid      = C.pack "thisisjustalittltest"
    , key      = C.pack "thisismytestkeyaight"
    , minPeers = 30
    , maxPeers = 55
    }

-- test :: String -> IO (Response B.ByteString)
test name = do
    bytes <- B.readFile name
    state <- openLocalState . fromRight $ torpify bytes
    url <- query state (MkURL global Nothing (Just Started))
    get url

fromRight (Right x) = x
