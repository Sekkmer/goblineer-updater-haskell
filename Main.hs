module Main where

import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy.IO as I
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

import Downloader (Auctions, Auction, Bonus, final)
import MarketValue (Items, Item)

instance FromJSON Bonus
instance FromJSON Auction
instance FromJSON Auctions

instance ToJSON Item
instance ToJSON Items

jsonURL :: String
jsonURL = "http://auction-api-eu.worldofwarcraft.com/auction-data/ad9b89618cc65dbf04bb7757fd7d2f38/auctions.json"

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

main :: IO ()
main = do 
    d <- (eitherDecode <$> getJSON) :: IO (Either String Auctions)
        case d of
        Left e      -> print "error"
        Right stuff -> I.writeFile "out.json" (encodeToLazyText (final stuff))
