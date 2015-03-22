{-# LANGUAGE DeriveDataTypeable#-}

module Curry.Prelude
    ( PieceMap
    , Addr(..)
    , Noitpecxe(..)
    --
    , mkReader
    , mkParser
    , perhaps
    --
    , eitherToMaybe
    , maybeToEither
    , extract
    , modifyTMVar
    --
    , (<%>)
    , (<+>)
    --
    , Chunk(..)
    , Chunkable(..)
    ) where

import qualified Data.ByteString as B
import           Data.Typeable
import qualified Data.Map as M
import           Data.Word
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Exception

----------------------------------------
-- TYPES
----------------------------------------

type PieceMap = M.Map Word32 Bool

data Addr = Addr
    { addrIp   :: String
    , addrPort :: String
    } deriving (Show, Eq)

data Noitpecxe = Noitpecxe String deriving (Show, Typeable)

instance Exception Noitpecxe

----------------------------------------
-- PARSER UTILS
----------------------------------------

mkReader :: Parser a -> B.ByteString -> Maybe a
mkReader parser bytes = case parse parser bytes `feed` B.empty of
    Done i r -> if null i then Just r else Nothing
    _ -> Nothing

mkParser :: (B.ByteString -> Maybe a) -> B.ByteString -> Parser a
mkParser reader bytes = case reader bytes of
    Just x -> return x
    Nothing -> empty

perhaps :: Bool -> Parser a -> Parser a
perhaps True  p = p
perhaps False _ = empty

----------------------------------------
-- MISC UTILS
----------------------------------------

extract :: (a -> Either String b) -> a -> IO b
extract f x = case f x of
    (Left  str) -> throw $ PatternMatchFail str
    (Right val) -> return val

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither _ (Just x) = Right x
maybeToEither s Nothing  = Left s

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left  _) = Nothing

modifyTMVar :: TMVar a -> (a -> a) -> STM ()
modifyTMVar v f = takeTMVar v >>= (putTMVar v . f)

----------------------------------------
-- INFIX UTILS
----------------------------------------

infixl 1 <%>
(<%>) :: Applicative f => f (a -> b) -> a -> f b
(<%>) = (. pure) . (<*>)

-- This is mplus, but (Either String) is already an instance
-- for SOME e in certain modules, so trying to use the actual
-- mplus class made things pretty messy.
infixl 6 <+>
(<+>) :: Either a b -> Either a b -> Either a b
r@(Right _) <+> _ = r
_ <+> r@(Right _) = r
_ <+> l@(Left  _) = l

----------------------------------------
-- CHUNKS
----------------------------------------

-- Information about a part of a piece
data Chunk a = Chunk
    { index :: Word32
    , start :: Word32
    , body  :: a
    } deriving (Show, Ord)

instance Functor Chunk where
    fmap f (Chunk i s b) = Chunk i s (f b)

instance Chunkable a => Eq (Chunk a) where
    x == y =  index x == index y
           && start x == start y
           && body x -=- body y

class Chunkable a where
    (-=-) :: Chunkable a => a -> a -> Bool

instance Chunkable Word32 where
    (-=-) = (==)

instance Chunkable B.ByteString where
    x -=- y = B.length x == B.length y

----------------------------------------
-- DEBUGGING
----------------------------------------

-- To allow STM types to instantiate show (which will only be used for debugging)

instance Show (TVar a) where
    show _ = "(a tvar exists here)"

instance Show (TMVar a) where
    show _ = "(a tmvar exists here)"

instance Show (TChan a) where
    show _ = "(a tchan exists here)"
