{-# LANGUAGE OverloadedStrings #-}
module App (app) where


import Servant.Server                 (serve, Application)
import Database.SQLite.Simple         (Connection)

import qualified Api


app :: Connection -> Application
app conn = serve Api.api (Api.combinedServer conn)
