{-# LANGUAGE OverloadedStrings #-}


module Util where

import Crypto.PasswordStore     (makePassword)
import Data.Aeson               (decode, encode)
import Data.Text.Encoding       (decodeUtf8)
import Network.Wai.Test         (SResponse (..))
import Test.Hspec.Wai           (request, WaiSession (..))
import Test.Hspec.Wai.Internal  (withApplication)

import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as Char8
import qualified Data.ByteString.Lazy   as BL
import qualified Database.SQLite.Simple as Sql

import qualified App
import qualified Model
import qualified Storage




addTestUserToDB :: Sql.Connection -> IO()
addTestUserToDB connection = do
  Storage.createSchema connection
  hash <- makePassword "testPassword" 17
  Sql.executeNamed connection "INSERT INTO user (name, password) VALUES ('test', :hash)"
    [":hash" Sql.:= decodeUtf8 hash]

createCredentialsJson :: String -> String -> BL.ByteString
createCredentialsJson user password =
    encode (Model.Credentials {Model.username = user, Model.password = password})


getJwtFromResponse :: SResponse -> B.ByteString
getJwtFromResponse response =
  case decode (simpleBody response) of
    Just jwt -> Char8.pack $ Model.token jwt
    Nothing -> ""

makeJwtRequest :: WaiSession SResponse
makeJwtRequest = request
  "POST"
  "/jwt"
  [("Content-Type", "application/json")]
  (createCredentialsJson "test" "testPassword")

addUserAndCheck :: Sql.Connection -> WaiSession () -> IO ()
addUserAndCheck connection action = do
  addTestUserToDB connection
  withApplication (App.app connection) action

setupAndCheck :: Sql.Connection -> (Sql.Connection -> IO()) -> WaiSession () -> IO ()
setupAndCheck connection dbAction action = do
  addTestUserToDB connection
  dbAction connection
  withApplication (App.app connection) action

