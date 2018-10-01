module MarketValue where

percent :: Int -> Double -> Int
percent n p = round (fromIntegral n * p / 100)
    
devide :: Double -> Int -> Double
devide a b = a / (fromIntegral b)

convertImpl :: [Double] -> Int -> Int -> Double -> ([Double], Double, Int) -> ([Double], Double, Int)
convertImpl []   _   _   _       (ret, sum, i) = (ret, sum `devide` i, i) 
convertImpl list min max percent (ret, sum, i)
    | i >=  max = return
    | i <  min  = recall
    | otherwise = 
        if y * percent >= x
            then recall
            else return
    where 
        y      = head ret
        x      = head list
        xs     = drop 1 list
        recall = convertImpl xs min max percent ([x] ++ ret, sum + x, (i + 1))
        return = (ret, sum `devide` i, i)

convert :: [Double] -> Int -> Int -> Double -> ([Double], Double, Int)
convert list min max percent = convertImpl list min max percent ([], 0, 0)

convert_15_30_150 :: [Double] -> ([Double], Double, Int)
convert_15_30_150 list = convert list (len `percent` 15) (len `percent` 30) 1.5
        where len = length list

standardDiv :: ([Double], Double, Int) -> Double
standardDiv (list, avg, count) = sqrt ((sum (map subSquare list)) `devide` (count - 1)) 
        where subSquare x = (x - avg) ^ 2

convertFinal :: [Double] -> Int -> Int -> Double -> Double -> [Double]
convertFinal list min max percent deviation = filter grater convertedList
        where 
            tuple = convert list min max percent
            first (x, _, _) = x
            third (_, _, x) = x
            avg = third tuple
            convertedList = first tuple
            stdDiv = standardDiv tuple
            grater x = abs (x - (fromIntegral avg)) >= deviation * stdDiv

convertFinal_15_30_150_150 :: [Double] -> [Double]
convertFinal_15_30_150_150 list = convertFinal list (len `percent` 15) (len `percent` 30) 1.5 1.5
        where len = length list

average :: [Double] -> Double
average list = (sum list) / (fromIntegral (length list))     
