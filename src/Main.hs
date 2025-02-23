{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Discord
import Discord.Types
import qualified Data.Text as T
import MysticTutor.Deck (Deck, uploadDeck, listDecks)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)
import Discord.Interactions (OptionDataSubcommand)
import MysticTutor.BotConfig (BotConfig(..), loadConfig)
import MysticTutor.Logger (logMessage, setupLogger)

main :: IO ()
main = do
  -- Load bot configuration
  configResult <- loadConfig "config.yaml"
  case configResult of
    Left err -> error $ "Failed to load config.yaml: " ++ err
    Right config -> do
      -- Set up logging
      logger <- setupLogger
      logMessage logger "Loaded config successfully!"

      let token = botToken config
      let logLevel = logLevel config

      -- Set up and run discord bot
      err <- runDiscord $ def
        { discordToken = token,
        discordOnEvent = eventHandler logger,
        discordGatewayIntent = def
        }

      logMessage logger ("Bot stopped with error: " <> show err)

-- A simple in-memory datbase for decks
decks :: IO (Map T.Text Deck)
decks = return Map.empty

-- Event Handler to process commands
eventHandler :: LoggerSet -> Event -> DiscordHandler ()
eventHandler logger (MessageCreate m) = do
  let userId = userId (messageAuthor m)
  let message = messageContent m

  -- Ping
  when (not (fromBot m) && message == "!ping") $ do
    liftIO $ logMessage logger "Received !ping command"
    _ <- restCall (CreateMessage (messageChannelId m) "Pong!")
    liftIO $ logMessage logger "Responded with Pong!"

  -- Check for !deck upload command
  -- when (not (fromBot m) && T.isPrefixOf "!deck upload" message) $ do
  --   let deckList = T.strip . T.drop 13 $ message -- Skip the "!deck upload " part
  --   updatedDecks <- liftIO $ uploadDeck userId deckList decks
  --   liftIO $ putStrLn $ "Deck uploaded: " ++ show updatedDecks
  --   _ <- restCall (CreateMessage (messageChannelId m) "Deck uploaded successfully!")

  -- Check for !deck list command
  -- when (not (fromBot m) && T.isPrefixOf "!deck list" message) $ do
  --   maybeDeck <- liftIO $ listDecks userId decks
  --   case maybeDeck of
  --     Just deck -> _ <- restCall (CreateMessage (messageChannelId m) (T.unlines deck))
  --     Nothing -> _ <- restCall (CreateMessage (messageChannelId m) "You don't have any decks uploaded.")

eventHandler _ _ = pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)
