{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
    ( Item(..)
    , Key(..)
    ) where

import Data.Text.Lazy (Text)
import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), object, (.:), (.=))

data Key = Key { key :: String }
    deriving (Read, Show)

data Item a = Item { description :: String
                   , color       :: String
                   , size        :: String
                   , image       :: a
                   }
    deriving (Read, Show)

instance ToJSON (Key) where
    toJSON (Key k) = object ["key" .= k]

instance FromJSON (Item Text) where
    parseJSON (Object v) = Item <$> v .: "description"
                                <*> v .: "color"
                                <*> v .: "size"
                                <*> v .: "photo"
    parseJSON _          = mzero
