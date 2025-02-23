{-# LANGUAGE DeriveGeneric #-}

module MysticTutor.Deck (Deck (..), uploadDeck, listDecks) where

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
type Decks = Map T.Text Deck

-- Function to upload a Deck
uploadDeck :: T.Text -> T.Text -> IO Decks -> IO Decks
uploadDeck userId newDeckList decks = do
  let cards = T.lines newDeckList -- Split by newlines
  let newDeck = Deck {deckName = userId, deckList = cards}
  Map.insert userId newDeck <$> decks

-- Function to list a user's Deck
listDecks :: T.Text -> IO Decks -> IO (Maybe [T.Text])
listDecks userId decks = do
  fmap deckList . Map.lookup userId <$> decks
