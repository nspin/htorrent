module Testing where

import           Bencode
import           Torrent
import           Control.Monad
import qualified Data.ByteString.Char8 as B

test :: IO ()
test = do
    file <- ifile
    print $ readBen file >>= torrentize

ifile = B.readFile "/home/nick/stuff/[kickass.so]nicki.minaj.pinkprint.deluxe.mp3.2014.torrent"
-- thefile = return $ B.pack $ "d8:announcekk
