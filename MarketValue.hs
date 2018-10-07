{-# LANGUAGE DeriveGeneric #-}

module MarketValue where

import Data.List (sort)
import GHC.Generics (Generic)

(%) :: Int -> Double -> Int
n % p = round (fromIntegral n * p / 100)

devide :: Double -> Int -> Double
devide a b = a / (fromIntegral b)

convertImpl :: [Double] -> Int -> Int -> Double -> ([Double], Double, Int) -> ([Double], Double, Int)
convertImpl []   _   _   _       (ret, sum, i) = (ret, sum `devide` i, i)
convertImpl list min max percent (ret, sum, i)
    | i >= max  = return
    | i <  min  = recall
    | ret == [] = recall
    | otherwise =
        if y * percent >= x
            then recall
            else return
    where
        y      = head ret
        x      = head list
        xs     = tail list
        recall = convertImpl xs min max percent (x : ret, sum + x, (i + 1))
        return = (ret, sum `devide` i, i)

convert :: [Double] -> Int -> Int -> Double -> ([Double], Double, Int)
convert list min max percent = convertImpl list min max percent ([], 0, 0)

standardDiv :: ([Double], Double, Int) -> Double
standardDiv (list, avg, count) = sqrt ((sum (map subSquare list)) `devide` (count - 1))
    where subSquare x = (x - avg) * (x - avg)

convertFinal :: [Double] -> Int -> Int -> Double -> Double -> [Double]
convertFinal list min max percent deviation = filter grater cvdList
    where
        tuple    = convert list min max percent
        avg      = (\(_, _, x) -> x) tuple
        cvdList  = (\(x, _, _) -> x) tuple
        stdDiv   = standardDiv tuple
        grater x = abs (x - (fromIntegral avg)) >= deviation * stdDiv

average :: [Double] -> Double
average list = (sum list) / (fromIntegral (length list))

mValue :: [Double] -> Int -> Double
mValue list len = average (convertFinal list min max 1.5 1.5)
    where
        min   = (len % 15)
        per30 = (len % 30)
        max   = if per30 <= 4 then 4 else per30

data Item = Item {
    item :: Int,
    marketvalue :: Double,
    min :: Double,
    quantity :: Int,
    bonusIds :: [Int]
} deriving (Show, Generic)

data Items = Items { items :: [Item] } deriving (Show, Generic)

toItems :: [Item] -> Items
toItems list = (Items list)

notEmpty :: [Int] -> Maybe [Int]
notEmpty [] = Nothing
notEmpty a  = Just    a

dataToItem :: ([Int], [Double]) -> Item
dataToItem (itemUID, []) = (Item (head itemUID) 0.0 0.0 0 (tail itemUID))
dataToItem (itemUID, list)
    | len == 1  = (Item id min min len bon)
    | otherwise = (Item id mVal min len bon)
    where
        len   = length list
        slist = sort list
        id    = head itemUID
        mVal  = mValue slist len
        min   = head slist
        bon   = tail itemUID
