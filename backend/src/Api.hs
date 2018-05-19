{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Api where


import Control.Monad.IO.Class               (liftIO)
import Database.SQLite.Simple               (Connection)
import Servant                              (Proxy (..), throwError)
import Servant.API.Alternative              ((:<|>) (..))
import Servant.API.Capture                  (Capture)
import Servant.API.ContentTypes             (JSON)
import Servant.API.Header                   (Header)
import Servant.API.ReqBody                  (ReqBody)
import Servant.API.Sub                      ((:>))
import Servant.API.Verbs                    (Get, Post)
import Servant.Server                       (Server, Handler)
import Servant.Server.Internal.ServantErr   (ServantErr, err401, err404, errBody)

import qualified Model as M
import qualified Logic as L



type JwtAPI =
  ReqBody '[JSON] M.Credentials :> Post '[JSON] M.Jwt

jwtServer :: Connection -> Server JwtAPI
jwtServer conn =
  grantJwt
    where
      grantJwt :: M.Credentials -> Handler M.Jwt
      grantJwt credentials = liftIOMaybeToHandler err $ L.issueJwt conn credentials
      err = err401 { errBody = "Wrong password or user does not exist."}


type PersonAPI =
       Capture "personId" Int :> Get '[JSON] M.Person
  :<|> "search" :> Capture "searchString" String :> Get '[JSON] [M.Person]
  :<|> Capture "personId" Int :> "tree" :> Get '[JSON] M.Person
  :<|> Capture "personId" Int :> "siblings" :> Get '[JSON] [[M.Person]]

personServer :: Connection -> Maybe M.JwtToken -> Server PersonAPI
personServer conn jwt =
    getPerson :<|> search :<|> getPersonTree :<|> getPersonSiblings
    where
      getPerson personId = withJwt jwt $ liftIOMaybeToHandler err404 $ L.getPersonById conn personId
      search searchString = withJwt jwt $ liftIO $ L.search conn searchString
      getPersonTree personId = withJwt jwt $ liftIOMaybeToHandler err404 $ L.getPersonTree conn personId
      getPersonSiblings personId = withJwt jwt $ liftIO $ L.findSiblings conn personId

withJwt :: Maybe M.JwtToken -> Handler a -> Handler a
withJwt jwt onValidJwt =
  case jwt of
    Just jwtToken -> do
      valid <- liftIO $ L.verifyJwtToken jwtToken
      if valid
        then onValidJwt
        else throwError err401 { errBody = "JWT token has expired or not valid." }
    Nothing -> throwError err401 { errBody = "Please provide JWT token in header." }


liftIOMaybeToHandler :: ServantErr -> IO (Maybe a) -> Handler a
liftIOMaybeToHandler err x = do
  m <- liftIO x
  case m of
    Nothing -> throwError err
    Just y -> return y



type API =
       "jwt" :> JwtAPI
  :<|> "person" :> Header "jwt" String :> PersonAPI

combinedServer :: Connection -> Server API
combinedServer conn =
       jwtServer conn
  :<|> personServer conn


api :: Proxy API
api = Proxy
