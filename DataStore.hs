{-# LANGUAGE ScopedTypeVariables #-}

module DataStore
    ( StoreCtx
    , withDataStore
    , getKeys
    , getItem
    , putItem
    ) where

import Numeric
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.ByteString (unpack)
import Data.ByteString.Lazy as BL (writeFile)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Base64.Lazy (decodeLenient)
import Crypto.Hash.SHA1 (hash)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath.Posix ((</>))


import Control.Monad (void)
import Control.Applicative ((<$>))
import Control.Exception (SomeException, throw, catch, bracket)
import Database.HDBC
import Database.HDBC.Sqlite3

import Types
import Config

data StoreCtx = StoreCtx Connection FilePath

table :: String
table = unwords
    [ "CREATE TABLE IF NOT EXISTS items"
    , "(key TEXT NOT NULL UNIQUE,"
    , "description TEXT,"
    , "color TEXT,"
    , "size TEXT,"
    , "image TEXT)"
    ]

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

withDataStore :: (StoreCtx -> IO a) -> IO a
withDataStore f = do
    createDirectoryIfMissing False imgFilePath
    withSqlConnection $ \c -> do
        void $ withTransaction c $ \c' -> run c' table []
        f $ StoreCtx c imgFilePath

withSqlConnection :: (Connection -> IO a) -> IO a
withSqlConnection = bracket (connectSqlite3 dbFilePath) disconnect

itemToSqlValue :: Item Key -> [SqlValue]
itemToSqlValue (Item d c s k) = map toSql [d, c, s, show k]

sqlValueToItem :: [SqlValue] -> Item Key
sqlValueToItem = (\(_:d:c:s:k:_) -> Item d c s $ read k) . map fromSql

makeKey :: Show a => a -> Key
makeKey = Key . concatMap (flip showHex "") . unpack . hash . pack . show

makeImageFilePath :: Key -> FilePath
makeImageFilePath (Key k) = imgFilePath </> k ++ ".jpg"

putImage :: FilePath -> Text -> IO ()
putImage path = BL.writeFile path . decodeLenient . encodeUtf8

putItemWithKey :: StoreCtx -> Key -> Item Key -> IO ()
putItemWithKey (StoreCtx c _) (Key k) items = void $ withTransaction c $ \c' ->
    run c' "INSERT INTO items VALUES (?, ?, ?, ?, ?);" $ values items
    where values = (toSql k :) . itemToSqlValue

getKeys :: StoreCtx -> IO [Key]
getKeys (StoreCtx c _) = fmap (map (Key . fromSql) . concat) $ withTransaction c $ \c' ->
    quickQuery' c' ("SELECT key FROM items") []

getItem :: StoreCtx -> Key -> IO (Maybe (Item FilePath))
getItem (StoreCtx c _) (Key k) =
    fmap convertSqlValue $ withTransaction c $ \c' ->
        quickQuery' c' ("SELECT * FROM items WHERE key == \"" ++ k ++ "\"") []
    where
    convertSqlValue v = imageFilePath . sqlValueToItem <$> safeHead v
    imageFilePath item = item { image = makeImageFilePath . image $ item }

putItem :: StoreCtx -> Item Text -> IO Key
putItem ctx item = do
    putImage imageFilePath (image item)
    putItemWithKey ctx dbItemKey dbItem `catch` \(e :: SomeException) -> do
        removeFile imageFilePath
        throw e
    return dbItemKey
    where
    dbItem = item { image = imageKey }
    dbItemKey = makeKey dbItem
    imageKey = makeKey $ image item
    imageFilePath = makeImageFilePath imageKey
