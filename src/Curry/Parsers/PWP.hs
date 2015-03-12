module Curry.Parsers.PWP
    ( Message(..)
    , Context
    , getBigEnd
    , getMsg
    , getCtxt
    , mkBigEnd
    , mkMsg
    , mkCtxt
    , emptyCtxt
    , filterMsg
    , merge
    ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.List hiding (take)
import           Data.List.Split
import qualified Data.Map as M
import           Data.Word
import           Prelude hiding (take)

----------------------------------------
-- TYPES
----------------------------------------

data Message = Keepalive
             | Choke
             | Unchoke
             | Interested
             | Bored
             | Have Integer
             | Bitfield (M.Map Integer Bool)
             | Request Integer Integer Integer
             | Piece Integer Integer B.ByteString
             | Cancel Integer Integer Integer
             deriving Show

data Context = Context
    { bep001 :: Bool
    , bep002 :: Bool
    , bep003 :: Bool
    , bep004 :: Bool
    , bep005 :: Bool
    , bep006 :: Bool
    } deriving Show

----------------------------------------
-- GETTERS
----------------------------------------

-- Parse a big-endian 4-bit integer
getBigEnd :: B.ByteString -> Either String Integer
getBigEnd = eitherResult . parse bigEnd

getMsg :: B.ByteString -> Either String Message
getMsg = eitherResult . (`feed` B.empty) . parse parseMsg

parseMsg = endOfInput *> return Keepalive <|> do
    msgID <- anyWord8
    case msgID of
        0 -> return Choke
        1 -> return Unchoke
        2 -> return Interested
        3 -> return Bored
        4 -> Have <$> bigEnd
        5 -> (Bitfield . unBitField) <$> takeByteString
        6 -> liftA3 Request bigEnd bigEnd bigEnd
        7 -> liftA3 Piece bigEnd bigEnd takeByteString
        8 -> liftA3 Cancel bigEnd bigEnd bigEnd

getCtxt :: B.ByteString -> Either String Context
getCtxt _ = Right $ Context False False False False False False

----------------------------------------
-- MAKERS
----------------------------------------

-- Turn an integer into a big-endian 4-bit bytestring.
-- Oversized ints are not handled.
mkBigEnd :: Integer -> B.ByteString
mkBigEnd int = B.pack [ fromIntegral $ 255 .&. shiftR int part
                      | part <- [24, 16, 8, 0]
                      ]

mkMsg :: Message -> B.ByteString
mkMsg (Keepalive     ) = B.empty
mkMsg (Choke         ) = B.singleton 0
mkMsg (Unchoke       ) = B.singleton 1
mkMsg (Interested    ) = B.singleton 2
mkMsg (Bored         ) = B.singleton 3
mkMsg (Have     x    ) = 4 `B.cons` mkBigEnd x
mkMsg (Bitfield x    ) = 5 `B.cons` bitField x
mkMsg (Request  x y z) = 6 `B.cons` (B.concat . map mkBigEnd) [x, y, z]
mkMsg (Piece    x y z) = 7 `B.cons` B.concat [mkBigEnd x, mkBigEnd y, z]
mkMsg (Cancel   x y z) = 8 `B.cons` (B.concat . map mkBigEnd) [x, y, z]

mkCtxt = const . B.pack $ replicate 8 0

----------------------------------------
-- UTILS
----------------------------------------

filterMsg :: Context -> Message -> Either String Message
filterMsg conext msg = case msg of
    Keepalive     -> yup
    Choke         -> yup
    Unchoke       -> yup
    Interested    -> yup
    Bored         -> yup
    Have _        -> yup
    Bitfield _    -> yup
    Request _ _ _ -> yup
    Piece _ _ _   -> yup
    Cancel _ _ _  -> yup
  where
    yup = Right msg
    nope True = const $ Right msg
    nope False = Left

merge :: Context -> Context -> Context
merge _ = id

emptyCtxt :: Context
emptyCtxt = Context False False False False False False

----------------------------------------
-- HELPERS
----------------------------------------

-- parse a 4-byte big-endian integer
bigEnd = (sum . zipWith (*) (iterate (* 256) 1) . map fromIntegral . reverse . B.unpack) <$> take 4

bitField :: M.Map Integer Bool -> B.ByteString
bitField = B.pack . unBitList . map snd . M.toList

unBitField :: B.ByteString -> M.Map Integer Bool
unBitField bits = M.fromList . zip [0..] . concatMap bitList $ B.unpack bits

bitList :: Word8 -> [Bool]
bitList w = map (testBit w) [7,6..0] -- right?

unBitList :: [Bool] -> [Word8]
unBitList = map ( foldl (.|.) 0
                . map (bit . fst)
                . filter snd
                . zip [7, 6..]
                ) . chunksOf 8
