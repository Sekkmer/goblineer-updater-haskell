{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy.IO as I
import Data.Text.Lazy
import Data.Text.Internal
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import System.Console.CmdArgs

import Downloader (Info, Infos, Auctions, Auction, Bonus, final, transformInfo, getUrl)
import MarketValue (Items, Item)

instance FromJSON Info
instance FromJSON Infos
instance FromJSON Bonus
instance FromJSON Auction
instance FromJSON Auctions

instance ToJSON Item
instance ToJSON Items
instance ToJSON Info
instance ToJSON Infos

data Args = Args {
  region :: String,
  realm :: String,
  apikey :: String
} deriving (Show, Data, Typeable)
arguments = Args{region = def, realm = def, apikey = def}

apiJsonURL :: IO String
apiJsonURL = do
  a <- region <$> cmdArgs arguments
  b <- realm <$> cmdArgs arguments
  c <- apikey <$> cmdArgs arguments
  return $ "https://" ++ a ++ ".api.battle.net/wow/auction/data/"++ b ++"?locale=en_US&apikey="++ c

getApiJSON :: IO B.ByteString
getApiJSON = do apiJsonURL >>= simpleHttp

main :: IO ()
main = do
  d <- (eitherDecode <$> getApiJSON) :: IO (Either String Infos)
  case d of
    Left e      -> print "error"
    Right stuff -> getAuctions url
      where
        url = correctUrl (unpack (encodeToLazyText (getUrl (transformInfo stuff))))
        correctUrl url = replaceStr url "\"" ""
        getAuctions url = do
          d <- (eitherDecode <$> simpleHttp url) :: IO (Either String Auctions)
          case d of
            Left e      -> print "error"
            Right stuff -> I.writeFile "out.json" (encodeToLazyText (final stuff))

replaceStr :: String -> String -> String -> String
replaceStr [] old new = []
replaceStr str old new = loop str
  where
    loop [] = []
    loop str =
      let (prefix, rest) = Prelude.splitAt n str
      in
        if old == prefix
        then new ++ loop rest
        else Prelude.head str : loop (Prelude.tail str)
    n = Prelude.length old
