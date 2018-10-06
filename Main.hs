module Main where

import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy.IO as I
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

apiKey :: String
apiKey = "6szbehcvdvjn8eccd2s496sr92956c87"

apiJsonURL :: String
apiJsonURL = "https://eu.api.battle.net/wow/auction/data/ragnaros?locale=en_US&apikey=" ++ apiKey

getApiJSON :: IO B.ByteString
getApiJSON = simpleHttp apiJsonURL

main :: IO ()
main = do 
    d <- (eitherDecode <$> getApiJSON) :: IO (Either String Infos)
    case d of
        Left e      -> print "error"
        Right stuff -> print (encodeToLazyText (getUrl (transformInfo stuff)))

jsonURL :: String
jsonURL = "http://auction-api-eu.worldofwarcraft.com/auction-data/ad9b89618cc65dbf04bb7757fd7d2f38/auctions.json"

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

-- main :: IO ()
-- main = do 
--     d <- (eitherDecode <$> getJSON) :: IO (Either String Auctions)
--     case d of
--         Left e      -> print "error"
--         Right stuff -> I.writeFile "out.json" (encodeToLazyText (final stuff))