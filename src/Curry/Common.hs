{-# LANGUAGE RecordWildCards, FlexibleInstances, DeriveDataTypeable#-}

module Curry.Common
    ( Error
    , Addr(..)
    , modifyTMVar
    , extract
    , eitherToMaybe
    , maybeToEither
    , (<%>)
    , (<+>)
    , Noitpecxe(..)
    --
    -- , ChuteIn
    -- , ChuteOut
    -- , newChute
    -- , putChute
    -- , takeChute
    -- , MCtrl
    -- , MView
    -- , newMSplit
    -- , modifyMCtrl
    -- , readMView
    -- , CountCtrl
    -- , CountView
    -- , newCount
    -- , addCount
    -- , readCount
    --
    ) where

import qualified Data.ByteString as B
import           Data.Typeable
import           Control.Applicative
import           Control.Concurrent.Chan
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad

type Error = Either String

data Addr = Addr
    { addrIp   :: String
    , addrPort :: String
    } deriving (Show, Eq)

modifyTMVar :: TMVar a -> (a -> a) -> STM ()
modifyTMVar v f = takeTMVar v >>= (putTMVar v . f)

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

data Noitpecxe = Noitpecxe String deriving (Show, Typeable)

instance Exception Noitpecxe

----------------------------------------
-- CETERA
----------------------------------------

-- To allow types with MVars and TVars to allow show (which will only be
-- used for debugging)

instance Show (TVar a) where
    show _ = "(a tvar exists here)"

instance Show (TMVar a) where
    show _ = "(a tmvar exists here)"

instance Show (TChan a) where
    show _ = "(a tchan exists here)"

instance Show (Chan a) where
    show _ = "(a chan exists here)"

----------------------------------------
-- SIMPLE SAFELY WRAPPED CONCURRENT TYPES
----------------------------------------

-- These are all thread safe because ALL threads modifying the wrapped mvars
-- will use a single take and single put, so they are guarenteed to be atomic.

-- data ChuteIn  a = ChuteIn  (MVar [a]) deriving Show
-- data ChuteOut a = ChuteOut (MVar [a]) deriving Show

-- newChute :: IO (ChuteIn a, ChuteOut a)
-- newChute = do
--     mvar <- newMVar []
--     return (ChuteIn mvar, ChuteOut mvar)

-- putChute :: ChuteIn a -> a -> IO ()
-- putChute (ChuteIn mvar) x = do
--     xs <- takeMVar mvar
--     putMVar mvar (x:xs)

-- takeChute :: ChuteOut a -> IO [a]
-- takeChute (ChuteOut mvar) = do
--     xs <- takeMVar mvar
--     putMVar mvar []
--     return xs

-- data MCtrl a = MCtrl (MVar a) deriving Show
-- data MView a = MView (MVar a) deriving Show

-- newMSplit :: IO (MCtrl a, MView a)
-- newMSplit = do
--     mvar <- newEmptyMVar
--     return (MCtrl mvar, MView mvar)

-- readMView :: MView a -> IO a
-- readMView (MView mvar) = do
--     x <- takeMVar mvar
--     putMVar mvar x
--     return x

-- modifyMCtrl :: MCtrl a -> (a -> a) -> IO ()
-- modifyMCtrl (MCtrl mvar) f = do
--     x <- takeMVar mvar
--     putMVar mvar (f x)

-- data CountCtrl a = CountCtrl (MVar a) deriving Show
-- data CountView a = CountView (MVar a) deriving Show

-- newCount :: Num a => IO (CountCtrl a, CountView a)
-- newCount = do
--     mvar <- newMVar 0
--     return (CountCtrl mvar, CountView mvar)

-- addCount :: Num a => CountCtrl a -> a -> IO ()
-- addCount (CountCtrl mvar) y = do
--     x <- takeMVar mvar
--     putMVar mvar (x + y)

-- readCount :: CountView a -> IO a
-- readCount (CountView mvar) = do
--     x <- takeMVar mvar
--     putMVar mvar x
--     return x
