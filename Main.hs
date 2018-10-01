module Main where

import Data.Aeson
import Downloader

main :: IO ()
main = do 
 d <- (eitherDecode <$> getJSON) :: IO (Either String Auctions)
 case d of
  Left e      -> putStrLn "error"
  Right stuff -> mapM_ print (final stuff)
