module Curry.Local where

data ChunkReq = (Chunk, MVar (Either String B.ByteString))

chunkGetter :: Pieces -> Chan ChunkReq -> IO ()

chunkPutter :: Pieces -> Chan ChunkReq -> IO ()
