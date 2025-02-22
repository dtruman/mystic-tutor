{-# LANGUAGE DeriveGeneric #-}

module MysticTutor.BotConfig (BotConfig (..), loadConfig) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import qualified Data.Text as T
import Data.Yaml (ParseException, decodeFileEither)
import GHC.Generics (Generic)

-- Define the data type for bot configuration
data BotConfig = BotConfig
  { botToken :: T.Text, -- Bot token for Discord authentication
    logLevel :: T.Text -- Log leve (e.g., DEBUG, INFO, ERROR)
  }
  deriving (Show, Generic)

-- Automatically derive the FromJSON instance using GHC.Generics
instance FromJSON BotConfig

-- Function to load and parse the YAML configuration file
loadConfig :: FilePath -> IO (Either String BotConfig)
loadConfig configPath = do
  result <- decodeFileEither configPath
  return $ case result of
    Left err -> Left (show (err :: ParseException))
    Right val -> Right val
