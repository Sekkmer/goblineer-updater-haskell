{-# LANGUAGE DeriveDataTypeable #-}
module Asd where
import System.Console.CmdArgs

data Asd = Asd {region :: String}
              deriving (Show, Data, Typeable)

data Qwe = Qwe {realm :: String}
              deriving (Show, Data, Typeable)

sample = Asd{region = def}
qwe = Qwe{realm = def}

main = print =<< cmdArgs qwe