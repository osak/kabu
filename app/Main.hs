{-# LANGUAGE OverloadedStrings #-}
module Main where

import AlphaVantage.Api
import Config.Credential

main :: IO ()
main = do
   cred <- Config.Credential.loadFromFile "/home/osamu/data/credentials.yml"
   print cred
   history <- runAlphaVantage (alphaVantageApiKey cred) $
       callApi "TIME_SERIES_DAILY" "AMZN"
   print history
