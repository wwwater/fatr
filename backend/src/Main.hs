{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Control.Exception                    (bracket)
import Database.SQLite.Simple               (Connection,
                                            open,
                                            close)
import Network.Wai                          (Middleware)
import Network.Wai.Handler.Warp             (run)
import Network.Wai.Middleware.Cors          (cors,
                                            CorsResourcePolicy (..),
                                            corsOrigins,
                                            corsMethods,
                                            corsRequestHeaders,
                                            corsExposedHeaders,
                                            corsMaxAge,
                                            corsVaryOrigin,
                                            corsRequireOrigin,
                                            corsIgnoreFailures,
                                            simpleHeaders)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant

import qualified App
import qualified Storage


testConnect :: IO Connection
-- testConnect = Sql.open ":memory:"
testConnect = open "db/fatr.db"


withTestConnection :: (Connection -> IO a) -> IO a
withTestConnection cb =
  withConn $ \conn -> cb conn
  where
    withConn = bracket testConnect close


blogCors :: Middleware
blogCors = cors $ const (Just blogResourcePolicy)


blogResourcePolicy :: CorsResourcePolicy
blogResourcePolicy =
    CorsResourcePolicy
        { corsOrigins = Nothing -- gives you /*
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "HEAD", "OPTION"]
        , corsRequestHeaders = "jwt":simpleHeaders -- adds "Content-Type" to defaults
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }



main :: IO ()
main = do
  withTestConnection $ \conn ->  do
    Storage.createSchema conn
    run 8081 $ logStdout $ blogCors $ App.app conn
