module Config where

import Network.Wai.Handler.Warp (Port)

-- Port where the web service should listen on
servicePort :: Port
servicePort = 8080

-- FileName of sqlite db
dbFilePath :: FilePath
dbFilePath = "items.db"

-- Directory where images should be stored
imgFilePath :: FilePath
imgFilePath = "img"
