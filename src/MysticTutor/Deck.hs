{-# LANGUAGE DeriveGeneric #-}

module MysticTutor.Deck (Deck (..), uploadDeck, listDecks, decks) where

import Control.Concurrent (newMVar, readMVar)
import Control.Concurrent.MVar (MVar)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.Generics (Generic)

-- Define the structure of a Deck
data Deck = Deck
  { deckName :: T.Text, -- Name of the Deck
    deckList :: [T.Text] -- List of cards in the Deck
  }
  deriving (Show, Generic)

instance FromJSON Deck

instance ToJSON Deck

-- A simple in-memory database for decks
type Decks = MVar (Map T.Text Deck)

decks :: IO Decks
decks = newMVar Map.empty

-- Function to upload a Deck
uploadDeck :: T.Text -> T.Text -> Map T.Text Deck -> IO (Map T.Text Deck)
uploadDeck userId newDeckList mDecks = do
  let cards = T.lines newDeckList
  let newDeck = Deck {deckName = userId, deckList = cards}

  let updatedDecks = Map.insert userId newDeck mDecks

  return updatedDecks

-- Function to list a user's Deck
listDecks :: T.Text -> IO (MVar (Map T.Text Deck)) -> IO (Maybe [T.Text])
listDecks userId decksLi = do
  mDecks <- readMVar =<< decksLi
  return $ Map.lookup userId mDecks >>= Just . deckList
