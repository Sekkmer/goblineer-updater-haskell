{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Downloader where
 
import Data.Aeson
import Data.List (sort)
import Data.Map (Map, lookupGE, insert, delete, empty, toAscList)

-- import Data.Text
-- import Control.Applicative
-- import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

import MarketValue(Item, dataToItem)

data Bonus = Bonus {
    bonusListId :: Int
} deriving (Show, Generic)

instance FromJSON Bonus
instance ToJSON Bonus

-- data Modifier = Modifier {
--     type :: Int,
--     value :: Int
-- } deriving (Show, Generic)

data Auction = Auction {
    -- auc :: Int,
    item :: Int,
    -- owner :: String,
    -- ownerRealm :: String,
    -- bid :: Int,
    buyout :: Int,
    quantity :: Int,
    -- timeLeft :: String,
    -- rand :: Int,
    -- seed :: Int,
    -- context :: Int,
    bonusLists :: Maybe [Bonus]
    --modifiers :: Maybe [Modifier]
} deriving (Show, Generic)

instance FromJSON Auction
instance ToJSON Auction

data Auctions = Auctions { auctions :: [Auction] } deriving (Show, Generic)

instance FromJSON Auctions
instance ToJSON Auctions

jsonURL :: String
jsonURL = "http://auction-api-eu.worldofwarcraft.com/auction-data/ad9b89618cc65dbf04bb7757fd7d2f38/auctions.json"

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

bonusToInt :: Bonus -> Int
bonusToInt (Bonus b) = b

auctionGetId :: Auction -> [Int] 
auctionGetId (Auction i _ _ list) = case list of
    Nothing  -> [i] 
    Just val -> [i] ++ (map bonusToInt val)

devide :: Int -> Int -> Double
devide a b = (fromIntegral a) / (fromIntegral b)

toList :: Int -> Int -> [Double]
toList n count = replicate count ((n `devide` count) / 10000.0)

addAuction :: Map [Int] [Double] -> Auction -> Map [Int] [Double]
addAuction map (Auction i bout quan list)
    | bout <= 0 = map
    | quan <= 0 = map
    | otherwise = case lookupGE aucId map of
        Nothing     -> insert aucId (toList bout quan) map
        Just (k, v) -> insert aucId ((toList bout quan) ++ v) (delete k map) 
        where
            aucId = auctionGetId (Auction i bout quan list)

transform :: [Auction] -> Map [Int] [Double] -> Map [Int] [Double]
transform []     map = map
transform [x]    map = addAuction map x
transform (x:xs) map = transform xs (addAuction map x)
   
calculate :: [ ([Int], [Double]) ] -> [Item]
calculate list = map dataToItem list

final :: Auctions -> [Item]
final (Auctions aucts) = calculate (toAscList (transform aucts empty)) 
