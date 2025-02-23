{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Discord
import Discord.Types
import qualified Discord.Requests as R
import qualified Data.Text as T
import MysticTutor.Deck (Deck, uploadDeck, listDecks, decks)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)
import Discord.Interactions (OptionDataSubcommand)
import MysticTutor.BotConfig (BotConfig(..), loadConfig)
import MysticTutor.Logger (logMessage, setupLogger)
import System.Log.FastLogger (LoggerSet)
import Control.Concurrent.MVar (newMVar, modifyMVar, MVar, readMVar)

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
      let logLevelSetting = logLevel config

      -- Set up and run discord bot
      err <- runDiscord $ def
        { discordToken = token,
        discordOnEvent = eventHandler logger,
        discordGatewayIntent = def
        }

      logMessage logger ("Bot stopped with error: " <> show err)

-- Event Handler to process commands
eventHandler :: LoggerSet -> Event -> DiscordHandler ()
eventHandler logger (MessageCreate m) = do
  let userIdFound = userId (messageAuthor m)
  let message = messageContent m

  -- Ping
  when (not (fromBot m) && message == "!ping") $ do
    liftIO $ logMessage logger "Received !ping command"
    _ <- restCall (R.CreateMessage (messageChannelId m) "Pong!")
    liftIO $ logMessage logger "Responded with Pong!"

  -- Check for !deck upload command
  when (not (fromBot m) && T.isPrefixOf "!deck upload" message) $ do
    let deckList = T.strip . T.drop 13 $ message -- Skip the "!deck upload " part
    -- Upload deck to in-memory
    deckVar <- liftIO decks -- Get the MVar
    updatedDecks <- liftIO $ modifyMVar deckVar $ \decksMap -> do
      newDeck <- uploadDeck (T.pack . show $ userIdFound) deckList decksMap
      return (newDeck, newDeck) -- Returning new deck state
    liftIO $ logMessage logger $ "Deck uploaded: " <> T.pack (show updatedDecks)
    _ <- restCall (R.CreateMessage (messageChannelId m) "Deck uploaded successfully!")
    return ()

  -- Check for !deck list command
  when (not (fromBot m) && T.isPrefixOf "!deck list" message) $ do
    deckVar <- liftIO decks -- Get the MVar
    maybeDeckList <- liftIO $ listDecks userIdFound deckVar
    case maybeDeckList of
      Just deckList -> do
        let deckMessage = T.unlines deckList
        _ <- restCall (R.CreateMessage (messageChannelId m) deckMessage)
        return ()
      Nothing -> do
        _ <- restCall (R.CreateMessage (messageChannelId m) "You don't have any decks uploaded.")
        return ()

eventHandler _ _ = pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)
