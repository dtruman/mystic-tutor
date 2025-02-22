{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Yaml (decodeFileEither)
import Discord
import Discord.Requests
import Discord.Types
import GHC.Generics (Generic)
import GHC.IO.Handle.Types (Handle (FileHandle))
import System.Log.FastLogger

data BotConfig = BotConfig
  { botToken :: T.Text,
    logLevel :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON BotConfig where
  parseJSON = withObject "BotConfig" $ \v ->
    BotConfig
      <$> v .: "bot_token"
      <*> v .: "log_level"

main :: IO ()
main = do
  -- Load Config FileHandle
  configResult <- decodeFileEither "config.yaml"
  case configResult of
    Left err -> error $ "Failed to load config.yaml: " ++ show error
    Right config -> do
      -- Set Up logging
      logger <- newStdoutLoggerSet 4097
      logMessage logger "Loaded config successfully."

      let token = unpack (botToken config)

      err <-
        runDiscord $
          def
            { discordToken = tok,
              discordOnEvent = eventHandler logger,
              discordGatewayIntent = def
            }

      logMessage logger ("Bot stopped with error: " <> show err)
      rmLoggerSet logger

eventHandler :: LoggerSet -> Event -> DiscordHandler ()
eventHandler logger (MessageCreate m) = when (not (fromBot m) && messageContent m == "!ping") $ do
  liftIO $ logMessage logger "Received !ping command"
  _ <- restCall (CreateMessage (messageChannelId m) "Pong!")
  liftIO $ logMessage logger "Responded with Pong!"
eventHandler _ _ = pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

logMessage :: LoggerSet -> String -> IO ()
logMessage logger msg = do
  time <- getCurrentTime
  pushLogStrLn logger (toLogStr (show time <> " [MysticTutor] " <> msg))
