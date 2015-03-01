module Curry.Common
    ( ChuteIn
    , ChuteOut
    , newChute
    , putChute
    , takeChute
    , MCtrl
    , MView
    , newMSplit
    , modifyMCtrl
    , readMView
    , CountCtrl
    , CountView
    , newCount
    , addCount
    , readCount
    ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM

----------------------------------------
-- SIMPLE SAFELY WRAPPED CONCURRENT TYPES
----------------------------------------

-- These are all thread safe because ALL threads modifying the wrapped mvars
-- will use a single take and single put, so they are guarenteed to be atomic.

data ChuteIn  a = ChuteIn  (MVar [a]) deriving Show
data ChuteOut a = ChuteOut (MVar [a]) deriving Show

newChute :: IO (ChuteIn a, ChuteOut a)
newChute = do
    mvar <- newMVar []
    return (ChuteIn mvar, ChuteOut mvar)

putChute :: ChuteIn a -> a -> IO ()
putChute (ChuteIn mvar) x = do
    xs <- takeMVar mvar
    putMVar mvar (x:xs)

takeChute :: ChuteOut a -> IO [a]
takeChute (ChuteOut mvar) = do
    xs <- takeMVar mvar
    putMVar mvar []
    return xs

data MCtrl a = MCtrl (MVar a) deriving Show
data MView a = MView (MVar a) deriving Show

newMSplit :: IO (MCtrl a, MView a)
newMSplit = do
    mvar <- newEmptyMVar
    return (MCtrl mvar, MView mvar)

readMView :: MView a -> IO a
readMView (MView mvar) = do
    x <- takeMVar mvar
    putMVar mvar x
    return x

modifyMCtrl :: MCtrl a -> (a -> a) -> IO ()
modifyMCtrl (MCtrl mvar) f = do
    x <- takeMVar mvar
    putMVar mvar (f x)

data CountCtrl a = CountCtrl (MVar a) deriving Show
data CountView a = CountView (MVar a) deriving Show

newCount :: Num a => IO (CountCtrl a, CountView a)
newCount = do
    mvar <- newMVar 0
    return (CountCtrl mvar, CountView mvar)

addCount :: Num a => CountCtrl a -> a -> IO ()
addCount (CountCtrl mvar) y = do
    x <- takeMVar mvar
    putMVar mvar (x + y)

readCount :: CountView a -> IO a
readCount (CountView mvar) = do
    x <- takeMVar mvar
    putMVar mvar x
    return x

----------------------------------------
-- CETERA
----------------------------------------

-- To allow types with MVars and TVars to allow show (which will only be
-- used for debugging)

instance Show (MVar a) where
    show _ = "(an mvar exists here)"

instance Show (TVar a) where
    show _ = "(a tvar exists here)"
