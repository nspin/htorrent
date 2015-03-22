module Curry.Parsers.Common
    ( mkReader
    , mkParser
    , perhaps
    ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString
import qualified Data.ByteString as B

----------------------------------------
-- PARSER UTILS
----------------------------------------

mkReader :: Parser a -> B.ByteString -> Maybe a
mkReader parser bytes = case parse parser bytes `feed` B.empty of
    Done i r -> if B.null i then Just r else Nothing
    _ -> Nothing

mkParser :: (B.ByteString -> Maybe a) -> B.ByteString -> Parser a
mkParser reader bytes = case reader bytes of
    Just x -> return x
    Nothing -> empty

perhaps :: Bool -> Parser a -> Parser a
perhaps True  p = p
perhaps False _ = empty
