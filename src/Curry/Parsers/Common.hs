module Curry.Parsers.Common
    ( (<$?>)
    , mkReader
    , perhaps
    ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString
import qualified Data.ByteString as B

----------------------------------------
-- PARSER UTILS
----------------------------------------

infixl 4 <$?>

-- Fmap that can fail
(<$?>) :: (Alternative m, Monad m) => (a -> Maybe b) -> m a -> m b
f <$?> m = do
    x <- m
    case f x of
        Nothing -> empty
        Just y -> pure y

mkReader :: Parser a -> B.ByteString -> Maybe a
mkReader parser bytes = case parse parser bytes `feed` B.empty of
    Done i r -> if B.null i then Just r else Nothing
    _ -> Nothing

perhaps :: Bool -> Parser a -> Parser a
perhaps True  p = p
perhaps False _ = empty
