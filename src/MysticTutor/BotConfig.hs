{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MysticTutor.BotConfig (BotConfig (..), loadConfig) where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import qualified Data.Text as T
import Data.Yaml (decodeFileEither)
import Data.Yaml.Internal (ParseException)
import GHC.Generics (Generic)

-- Define the data type for bot configuration
data BotConfig = BotConfig
  { botToken :: T.Text, -- Bot token for Discord authentication
    logLevel :: T.Text -- Log leve (e.g., DEBUG, INFO, ERROR)
  }
  deriving (Show, Generic)

-- Automatically derive the FromJSON instance using GHC.Generics
instance FromJSON BotConfig where
  parseJSON = withObject "BotConfig" $ \v ->
    BotConfig
      <$> v .: "bot_token"
      <*> v .: "log_level"

-- Function to load and parse the YAML configuration file
loadConfig :: FilePath -> IO (Either String BotConfig)
loadConfig configPath = do
  result <- decodeFileEither configPath
  return $ case result of
    Left err -> Left (show (err :: ParseException))
    Right val -> Right val
