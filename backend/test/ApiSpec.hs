{-# LANGUAGE OverloadedStrings #-}


module ApiSpec (main, spec) where

import Test.Hspec               (hspec, Spec, beforeAll, afterAll, after, describe, it)
import Test.Hspec.Wai           (get, request, shouldRespondWith, matchStatus, WaiSession (..))
import Test.Hspec.Wai.Internal  (withApplication)

import qualified Database.SQLite.Simple as Sql

import qualified App
import qualified Storage



main :: IO ()
main = hspec spec


testConnect :: IO Sql.Connection
testConnect = Sql.open ":memory:"


spec :: Spec
spec = beforeAll testConnect $
       afterAll Sql.close $ do

  describe "test GET /person endpoint" $
    after (\connection -> Sql.execute_ connection "DROP TABLE person") $ do

    it "person not found" $ \connection -> do
      Storage.createSchema connection
      withApplication (App.app connection) $ do
        get "/person/1" `shouldRespondWith` 404

    it "retrieves person" $ \connection -> do
      Storage.createSchema connection
      Sql.execute_ connection "INSERT INTO person (givenName, surname, parents, children) VALUES \
        \('Max', 'Planck', '{\"motherId\":2,\"fatherId\":3}', '[]')"
      withApplication (App.app connection) $ do
        get "/person/1" `shouldRespondWith`
          "{\"givenName\":\"Max\",\
           \\"deathday\":null,\
           \\"patronymic\":null,\
           \\"children\":[],\
           \\"birthday\":null,\
           \\"parents\":{\"father\":null,\"mother\":null},\
           \\"surname\":\"Planck\"}"
          {matchStatus = 200}

    it "retrieves person's ancestors" $ \connection -> do
      Storage.createSchema connection
      Sql.execute_ connection "INSERT INTO person (id, givenName, surname, parents, children) VALUES \
        \(1, 'Max', 'Planck', '{\"motherId\":2,\"fatherId\":3}', '[]')"
      Sql.execute_ connection "INSERT INTO person (id, givenName, surname, parents, children) VALUES \
        \(3, 'Johann', 'Planck', '{\"motherId\":4,\"fatherId\":5}', '[]')"
      withApplication (App.app connection) $ do
        get "/person/1/ancestors" `shouldRespondWith`
          "{\"givenName\":\"Max\",\
           \\"deathday\":null,\
           \\"patronymic\":null,\
           \\"children\":[],\
           \\"birthday\":null,\
           \\"parents\":\
            \{\"father\":{\"givenName\":\"Johann\",\
                         \\"deathday\":null,\
                         \\"patronymic\":null,\
                         \\"children\":[],\
                         \\"birthday\":null,\
                         \\"parents\":{\"father\":null,\"mother\":null},\
                         \\"surname\":\"Planck\"},\
             \\"mother\":null},\
           \\"surname\":\"Planck\"}"
          {matchStatus = 200}

