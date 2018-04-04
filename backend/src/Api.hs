{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Api where


import Servant                              (Proxy (..), throwError)
import Servant.Server                       (Server, Handler)
import Servant.API.Alternative              ((:<|>) (..))
import Servant.API.Sub                      ((:>))
import Servant.API.Verbs                    (Get)
import Servant.API.Capture                  (Capture)
import Servant.API.ContentTypes             (JSON)
import Servant.Server.Internal.ServantErr   (ServantErr, err404)
import Control.Monad.IO.Class               (liftIO)
import Database.SQLite.Simple               (Connection)

import qualified Model as M
import qualified Logic as L



type PersonAPI =
       Capture "personId" Int :> Get '[JSON] M.Person
  :<|> "search" :> Capture "searchString" String :> Get '[JSON] [M.Person]
  :<|> Capture "personId" Int :> "ancestors" :> Get '[JSON] M.Person
  :<|> Capture "personId" Int :> "descendants" :> Get '[JSON] M.Person

personServer :: Connection ->  Server PersonAPI
personServer conn =
    getPerson :<|> search :<|> getAncestors :<|> getDescendants
    where
      getPerson personId = liftIOMaybeToHandler err404 $ L.getPersonById conn personId
      search searchString = liftIO $ L.search conn searchString
      getAncestors personId = liftIOMaybeToHandler err404 $ L.getAncestors conn personId
      getDescendants personId = liftIOMaybeToHandler err404 $ L.getDescendants conn personId


liftIOMaybeToHandler :: ServantErr -> IO (Maybe a) -> Handler a
liftIOMaybeToHandler err x = do
  m <- liftIO x
  case m of
    Nothing -> throwError err
    Just y -> return y




type API =
  "person" :> PersonAPI

combinedServer :: Connection -> Server API
combinedServer conn =
  personServer conn


api :: Proxy API
api = Proxy
