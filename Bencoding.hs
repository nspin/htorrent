module Bencoding ( readBen
                 , writeBen
                 ) where

import           Data.Map
import           Text.Parsec
import qualified Data.List.NonEmpty as N
import qualified Data.ByteString    as B

data BData = BDict (NonEmpty (ByteString BData))
           | BList (NonEmpty Integer)
           | BInteger Integer
           | BString ByteString

parseBen = do

