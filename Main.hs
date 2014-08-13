{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Data.Aeson (encode)
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.FilePath ((</>))
import Web.Scotty

import Types
import Config
import DataStore

jsonItem :: ActionM (Item Text)
jsonItem = jsonData

itemsHtml :: [Key] -> Text
itemsHtml = pack . unlines . map toHtml
    where toHtml (Key k) = "<a href=item/" ++ k ++ ">" ++ k ++ "</a><br>"

itemHtml :: Maybe (Item FilePath) -> Text
itemHtml Nothing               = decodeUtf8 . encode $ Error 404 "item not found" Nothing
itemHtml (Just (Item d c s p)) = mconcat
    [ "<table border=\"1\">"
    , "<tr>"
    , "<td>"
        , "<img alt=\"Photo\" src=/", pack p, "></img>"
    , "</td>"
    , "<td>"
        , "<table border=\"1\">"
        , "<tr>", "<td>Description</td>", "<td>", pack d, "</td>", "</tr>"
        , "<tr>", "<td>Color      </td>", "<td>", pack c, "</td>", "</tr>"
        , "<tr>", "<td>Size       </td>", "<td>", pack s, "</td>", "</tr>"
        , "</table>"
    , "</td>"
    , "</tr>"
    , "</table>"
    ]

main :: IO ()
main = withDataStore $ \ctx -> scotty servicePort $ do
    -- Simple JSON error handler for 500 (exceptions)
    defaultHandler   $ json . Error 500 "internal server error" . Just
    -- POSTing an item
    post "/"         $ jsonItem >>= liftIO . putItem ctx >>= json
    -- GETting a list of items
    get "/items"     $ liftIO (getKeys ctx) >>= html . itemsHtml
    -- GETting a single item
    get "/item/:key" $ param "key" >>= liftIO . getItem ctx . Key >>= html . itemHtml
    -- GETting an image
    get (capture $ "/" ++ imgFilePath ++ "/:img") $ param "img" >>= file . (imgFilePath </>)
    -- Simple JSON error handler for 400 (not found)
    notFound         $ json $ Error 400 "service not found" Nothing
