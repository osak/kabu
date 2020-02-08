{-# LANGUAGE OverloadedStrings #-}

module AlphaVantage.Api where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types     (Parser)
import qualified Data.HashMap.Strict  as H
import           Data.List
import           Data.Maybe
import           Data.Text            (Text, unpack)
import           Data.Time
import           Network.HTTP.Req
import           Text.Printf

newtype Config =
  Config
    { apiKey :: Text
    }

data StockHistory =
  StockHistory
    { metadata :: Metadata
    , history  :: [Price]
    }
  deriving (Show)

data Metadata =
  Metadata
    { symbol        :: Text
    , lastRefreshed :: Day
    }
  deriving (Show)

data Price =
  Price
    { day    :: Day
    , open   :: Double
    , high   :: Double
    , low    :: Double
    , close  :: Double
    , volume :: Int
    }
  deriving (Show)

instance FromJSON StockHistory where
  parseJSON = withObject "StockHistory" $ \v -> StockHistory <$> v .: "Meta Data" <*> (fetchSeries v >>= parseSeries)
    where
      fetchSeries :: Object -> Parser Value
      fetchSeries v = do
        a <- v .:? "Time Series (Daily)"
        b <- v .:? "Monthly Series"
        maybe (fail "Cannot find series") (return . fromJust) $ find isJust [a, b]
      parseSeries :: Value -> Parser [Price]
      parseSeries = withObject "Series" $ \v -> sortOn day <$> mapM (uncurry parsePrice) (H.toList v)
      parsePrice :: Text -> Value -> Parser Price
      parsePrice day =
        withObject "Price" $ \v ->
          Price (fromJust (parseTimeM True defaultTimeLocale "%Y-%m-%d" (unpack day) :: Maybe Day)) <$>
          fmap read (v .: "1. open") <*>
          fmap read (v .: "2. high") <*>
          fmap read (v .: "3. low") <*>
          fmap read (v .: "4. close") <*>
          fmap read (v .: "5. volume")

instance FromJSON Metadata where
  parseJSON = withObject "Metadata" $ \v -> Metadata <$> v .: "2. Symbol" <*> v .: "3. Last Refreshed"

runAlphaVantage :: Text -> Reader Config (IO a) -> IO a
runAlphaVantage apiKey reader = runReader reader (Config apiKey)

callApi :: Text -> Text -> Reader Config (IO StockHistory)
callApi function symbol = asks (runAndParse . apiKey)
  where
    runAndParse :: Text -> IO StockHistory
    runAndParse key = do
      r <-
        runReq defaultHttpConfig $
        req GET (https "www.alphavantage.co" /: "query") NoReqBody jsonResponse $
        "function" =: function <> "symbol" =: symbol <> "outputsize" =: ("full" :: Text) <> "apikey" =: key
      return (responseBody r :: StockHistory)
