{-# LANGUAGE DeriveGeneric #-}

module Downloader where
 
import Data.Map (Map, lookupGE, insert, delete, empty, toAscList)
import GHC.Generics (Generic)

import MarketValue(Items, Item, dataToItem, toItems)

data Info = Info {
    url :: String,
    lastModified :: Int
} deriving (Show, Generic)

data Infos = Infos { files :: [Info] } deriving (Show, Generic)

data Bonus = Bonus {
    bonusListId :: Int
} deriving (Show, Generic)

data Auction = Auction {
    item :: Int,
    buyout :: Int,
    quantity :: Int,
    bonusLists :: Maybe [Bonus]
} deriving (Show, Generic)

data Auctions = Auctions { auctions :: [Auction] } deriving (Show, Generic)

bonusToInt :: Bonus -> Int
bonusToInt (Bonus b) = b

auctionGetId :: Auction -> [Int] 
auctionGetId (Auction i _ _ list) = case list of
    Nothing  -> [i] 
    Just val -> i : (map bonusToInt val)

devide :: Int -> Int -> Double
devide a b = (fromIntegral a) / (fromIntegral b)

toList :: Int -> Int -> [Double]
toList n count = replicate count (n `devide` count) 

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

final :: Auctions -> Items
final (Auctions aucts) = toItems (calculate (toAscList (transform aucts empty)))




transformInfo :: Infos -> Info
transformInfo (Infos (info:infos)) = info

getUrl :: Info -> String
getUrl (Info x _) = x
