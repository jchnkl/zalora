{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Data.Text.Lazy (Text, pack)
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

itemHtml :: Item FilePath -> Text
itemHtml (Item d c s p) = mconcat
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
    post "/" $ jsonItem >>= liftIO . putItem ctx >>= json
    get "/items" $ liftIO (getKeys ctx) >>= html . itemsHtml
    get "/item/:key" $ param "key" >>= liftIO . getItem ctx . Key >>= html . itemHtml
    get (capture $ "/" ++ imgFilePath ++ "/:img") $ param "img" >>= file . (imgFilePath </>)
