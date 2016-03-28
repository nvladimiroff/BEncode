{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module BEncode where

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 as BA
import Data.Foldable (fold)
import Data.Monoid ((<>))
import Network.HTTP.Types.URI (urlEncode)
import Crypto.Hash.SHA1 (hash)
import Control.Applicative

import Control.Lens

{- Example usage
> x <- B.readFile "torrent.torrent"
> x^? bencoded . ix "info" . ix "name" . _BString
Just "ubuntu-14.04-desktop-amd64.iso"
-}
data BEncode = BString B.ByteString
             | BInt Integer
             | BList [BEncode]
             | BDict (M.Map B.ByteString BEncode)
    deriving (Show, Read, Eq, Ord)

type instance Index BEncode = B.ByteString
type instance IxValue BEncode = BEncode
instance Ixed BEncode where
    ix k f m@(BString _) = pure m
    ix k f m@(BInt    _) = pure m
    ix k f m@(BList   _) = pure m
    ix k f m@(BDict   d) = case M.lookup k d of
        Just v -> f v <&> \v' -> BDict (M.insert k v' d)
        Nothing -> pure m

makePrisms ''BEncode

encode :: BEncode -> B.ByteString
encode (BString bs) = (B.pack $ show $ B.length bs) <> ":" <> bs
encode (BInt i)     = "i" <> B.pack (show i) <> "e"
encode (BList xs)   = "l" <> fold (map encode xs) <> "e"
encode (BDict m)    = "d" <> flatten m <> "e"
  where
      flatten = fold . M.mapWithKey (\k a -> (B.pack $ show $ B.length k) <> ":" <> k <> encode a)

bencoded :: Simple Prism B.ByteString BEncode
bencoded = prism encode $ \x ->
    case parseOnly parsebenc x of
        Left _ -> Left x
        Right a -> Right a

infoHash :: Fold BEncode B.ByteString
infoHash = ix "info" . re bencoded . to (urlEncode True . hash)

parsebenc :: Parser BEncode
parsebenc = bstring <|> bint <|> blist <|> bdict <?> "Couldn't parse bencode"

bstring :: Parser BEncode
bstring = do
    len <- decimal
    char ':'
    str <- BA.take len
    return (BString str)

bkey :: Parser B.ByteString
bkey = do
    len <- decimal
    char ':'
    str <- BA.take len
    return str

bint :: Parser BEncode
bint = do
    char 'i'
    num <- decimal
    char 'e'
    return (BInt num)

blist :: Parser BEncode
blist = do
    char 'l'
    list <- many parsebenc
    char 'e'
    return (BList list)

bdict :: Parser BEncode
bdict = do
    char 'd'
    dict <- many $ do
        key <- bkey
        val <- parsebenc
        return (key, val)
    char 'e'
    return . BDict $ M.fromList dict
