module Test where

import           Curtis.Track.Torrent
import           Curtis.Track.THP
import qualified Data.ByteString as B
import           Data.Digest.SHA1
import           Network.Wreq

initPeerId = Word160 52395 54325 54321 64352 6543
initPort = 7000

test :: IO ()
test = do
    file <- B.readFile "/home/nick/stuff/slackware-14.1-install-dvd.torrent"
    let Just t = torrentize file
        url =  mkURL $ initTHP initPeerId initPort t
    print url
    resp <- get url
    print resp
