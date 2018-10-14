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

type UItem = ([Int], [Double])
type UItemMap = Map [Int] [Double]

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

auctionToUItem :: Auction -> UItem
auctionToUItem a = (auctionGetId a, auctionGetList a)

insertPairWith :: Ord k => (a -> a -> a) -> (k, a) -> Map k a -> Map k a
insertPairWith inserter (k, a) = insertWith inserter k a

insertUItem :: UItem -> UItemMap -> UItemMap
insertUItem = insertPairWith (++) 

toUItemMap :: [Auction] -> UItemMap
toUItemMap = foldr' insertUItem (empty :: UItemMap) . map auctionToUItem

auctionsToItems :: Auctions -> Items
auctionsToItems = toItems . map dataToItem . toAscList . toUItemMap . auctions

getUrlFromInfos :: Infos -> String
getUrlFromInfos = url . head . files 
