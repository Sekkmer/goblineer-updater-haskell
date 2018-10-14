{-# LANGUAGE DeriveGeneric #-}

module Downloader where
 
import Data.Foldable (foldr')
import Data.Map (Map, empty, toAscList, insertWith)
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

devide :: Int -> Int -> Double
devide a b = (fromIntegral a) / (fromIntegral b)

auctionGetId :: Auction -> [Int] 
auctionGetId (Auction i _ _ list) = case list of
  Nothing  -> [i] 
  Just val -> i : (map bonusToInt val)

auctionGetList :: Auction -> [Double]
auctionGetList auct 
  | buyout auct == 0   = []
  | count == 0 = []
  | otherwise          = 
    replicate count ((buyout auct) `devide` count) 
    where 
      count = (quantity auct)

insertAuction :: Auction -> Map [Int] [Double] -> Map [Int] [Double]
insertAuction auct = insertWith (++) key value
  where
    key   = auctionGetId auct
    value = auctionGetList auct

transform :: [Auction] -> Map [Int] [Double]
transform = foldr' insertAuction inti 
  where
    inti :: Map [Int] [Double]
    inti = empty

calculate :: [ ([Int], [Double]) ] -> [Item]
calculate = map dataToItem

auctionsToItems :: Auctions -> Items
auctionsToItems = toItems . calculate . toAscList . transform . auctions

transformInfo :: Infos -> Info
transformInfo =  head . files

getUrl :: Info -> String
getUrl (Info x _) = x
