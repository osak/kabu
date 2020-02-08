{-# LANGUAGE OverloadedStrings #-}
module Config.Credential where

import qualified Data.Yaml as Y
import Data.HashMap.Strict ((!))
import Data.Yaml ((.:))
import Data.Text

newtype Credential = Credential{alphaVantageApiKey :: Text} deriving (Show)

loadFromFile :: FilePath -> IO Credential
loadFromFile path = do
    raw <- Y.decodeFileThrow path :: IO Y.Object
    let key = case raw ! "alphavantage" of
                Y.Object av -> case av ! "api_key" of
                                  Y.String key -> key
                                  _ -> error "Key 'api_key' must contain a string"
                _ -> error "Key 'alphavantage' must contain a map"
    return $ Credential key
