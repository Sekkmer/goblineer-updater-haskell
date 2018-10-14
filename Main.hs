{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Main where

import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy.IO (writeFile)
import Data.Text.Lazy (unpack)
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import System.Console.CmdArgs (Data, Typeable, cmdArgs, def)

import Prelude hiding (writeFile)

import Downloader (Info, Infos, Auctions, Auction, Bonus, auctionsToItems, transformInfo, getUrl)
import MarketValue (Items, Item)

instance FromJSON Info
instance FromJSON Infos
instance FromJSON Bonus
instance FromJSON Auction
instance FromJSON Auctions

instance ToJSON Item
instance ToJSON Items

data Args = Args {
  region :: String,
  realm :: String,
  apikey :: String
} deriving (Show, Data, Typeable)

arguments :: Args
arguments = Args{region = def, realm = def, apikey = def}

apiJsonURL :: IO String
apiJsonURL = do
  a <- region <$> cmdArgs arguments
  b <- realm <$> cmdArgs arguments
  c <- apikey <$> cmdArgs arguments
  return $ "https://" ++ a ++ ".api.battle.net/wow/auction/data/"++ b ++"?locale=en_US&apikey="++ c

getApiJSON :: IO B.ByteString
getApiJSON = apiJsonURL >>= simpleHttp
    
decodeAndDoRight :: FromJSON a => (a -> IO ()) -> IO B.ByteString -> IO ()
decodeAndDoRight fun response = do
  d <- (eitherDecode <$> response)
  case d of
    Left error  -> print error
    Right stuff -> fun stuff

main :: IO ()
main = decodeAndDoRight processInfos getApiJSON
  where
    getURL = filter ('"'/=) . unpack . encodeToLazyText . getUrl . transformInfo
    write = writeFile "out.json" . encodeToLazyText . auctionsToItems
    processInfos = decodeAndDoRight write . simpleHttp . getURL 
