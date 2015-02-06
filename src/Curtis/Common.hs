module Curtis.Common
    ( marse
    ) where

import           Control.Applicative

import           Data.Bits
import           Data.List
import           Data.Word
import qualified Data.ByteString as B
import           Data.Attoparsec.ByteString

marse :: Parser a -> B.ByteString -> Maybe a
marse parser = maybeResult . parse parser

