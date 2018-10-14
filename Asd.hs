{-# LANGUAGE DeriveDataTypeable #-}
module Asd where

import System.Console.CmdArgs
import System.IO.Unsafe
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B


data Args = Args {
    region :: String, 
    realm :: String,
    apikey :: String
} deriving (Show, Data, Typeable)


arguments = Args{region = def, realm = def, apikey = def}

region_ :: IO String
region_ = region <$> cmdArgs arguments

realm_ :: IO String
realm_ = realm <$> cmdArgs arguments

apikey_ :: IO String
apikey_ = apikey <$> cmdArgs arguments

apiJsonURL :: IO String
apiJsonURL = do 
    a <- region_
    b <- realm_
    c <- apikey_
    return $ "https://" ++ a ++ ".api.battle.net/wow/auction/data/"++ b ++"?locale=en_US&apikey="++ c

getApiJSON :: IO B.ByteString
getApiJSON = simpleHttp (unsafePerformIO apiJsonURL)

main = print =<< getApiJSON