{-# LANGUAGE OverloadedStrings #-}


module PersonApiSpec (main, spec) where

import Test.Hspec               (hspec, Spec, beforeAll, afterAll, after, describe, it)
import Test.Hspec.Wai           (request, shouldRespondWith, matchStatus)

import qualified Database.SQLite.Simple as Sql


import qualified App
import qualified Storage

import Util


main :: IO ()
main = hspec spec


testConnect :: IO Sql.Connection
testConnect = Sql.open ":memory:"


spec :: Spec
spec = beforeAll testConnect $
       afterAll Sql.close $ do

  describe "test GET /person/{id} endpoint" $
    after (\connection -> do
          Sql.execute_ connection "DROP TABLE person"
          Sql.execute_ connection "DROP TABLE user"
          ) $ do

    it "returns unauthorized without jwt" $ \connection ->
      addUserAndCheck connection $
        request "GET"
                "/person/1"
                []
                ""
        `shouldRespondWith` 401

    it "returns unauthorized with bad jwt" $ \connection ->
      addUserAndCheck connection $
        request "GET"
                "/person/1"
                [("Content-Type", "application/json"), ("jwt", "bad-jwt")]
                ""
        `shouldRespondWith` 401

    it "person not found" $ \connection ->
      addUserAndCheck connection $ do
        response <- makeJwtRequest
        request "GET"
                "/person/1"
                [("Content-Type", "application/json"), ("jwt", getJwtFromResponse response)]
                ""
        `shouldRespondWith` 404

    it "retrieves person" $ \connection ->
      setupAndCheck connection addTestPerson $ do
        response <- makeJwtRequest
        request "GET"
                "/person/1"
                [("Content-Type", "application/json"), ("jwt", getJwtFromResponse response)]
                ""
        `shouldRespondWith`
        "{\"givenName\":\"Max\",\
         \\"deathday\":null,\
         \\"patronymic\":null,\
         \\"children\":[],\
         \\"birthday\":null,\
         \\"parents\":{\"father\":null,\"mother\":null},\
         \\"id\":1,\
         \\"surname\":\"Planck\"}"
        {matchStatus = 200}

  describe "test GET /person/{id}/ancestors endpoint" $
    after (\connection -> do
          Sql.execute_ connection "DROP TABLE person"
          Sql.execute_ connection "DROP TABLE user"
          ) $ do

    it "returns unauthorized without jwt" $ \connection ->
      addUserAndCheck connection $
        request "GET"
                "/person/1/ancestors"
                []
                ""
        `shouldRespondWith` 401

    it "retrieves person's ancestors" $ \connection ->
      setupAndCheck connection addTestPerson $ do
        response <- makeJwtRequest
        request "GET"
                "/person/1/ancestors"
                [("Content-Type", "application/json"), ("jwt", getJwtFromResponse response)]
                ""
        `shouldRespondWith`
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
                         \\"id\":3,\
                         \\"surname\":\"Planck\"},\
             \\"mother\":null},\
           \\"id\":1,\
           \\"surname\":\"Planck\"}"
          {matchStatus = 200}

  describe "test GET /person/{id}/descendants endpoint" $
    after (\connection -> do
          Sql.execute_ connection "DROP TABLE person"
          Sql.execute_ connection "DROP TABLE user"
          ) $ do

    it "returns unauthorized without jwt" $ \connection ->
      addUserAndCheck connection $
        request "GET"
                "/person/1/descendants"
                []
                ""
        `shouldRespondWith` 401

    it "retrieves person's descendants" $ \connection ->
      setupAndCheck connection addTestPerson $ do
        response <- makeJwtRequest
        request "GET"
                "/person/3/descendants"
                [("Content-Type", "application/json"), ("jwt", getJwtFromResponse response)]
                ""
        `shouldRespondWith`
          "{\"givenName\":\"Johann\",\
           \\"deathday\":null,\
           \\"patronymic\":null,\
           \\"children\":[{\"spouse\":null,\
                          \\"childrenWithSpouse\":[\
                                  \{\"givenName\":\"Max\",\
                                   \\"deathday\":null,\
                                   \\"patronymic\":null,\
                                   \\"children\":[],\
                                   \\"birthday\":null,\
                                   \\"parents\":\
                                    \{\"father\":null,\
                                     \\"mother\":null},\
                                   \\"id\":1,\
                                   \\"surname\":\"Planck\"}]}],\
           \\"birthday\":null,\
           \\"parents\":{\"father\":null,\"mother\":null},\
           \\"id\":3,\
           \\"surname\":\"Planck\"}"
          {matchStatus = 200}


  describe "test GET /person/search/{str} endpoint" $
    after (\connection -> do
          Sql.execute_ connection "DROP TABLE person"
          Sql.execute_ connection "DROP TABLE user"
          ) $ do

    it "returns unauthorized without jwt" $ \connection ->
      addUserAndCheck connection $
        request "GET"
                "/person/search/max"
                []
                ""
        `shouldRespondWith` 401

    it "searches person" $ \connection ->
      setupAndCheck connection addTestPerson $ do
        response <- makeJwtRequest
        request "GET"
                "/person/search/max"
                [("Content-Type", "application/json"), ("jwt", getJwtFromResponse response)]
                ""
        `shouldRespondWith`
          "[{\"givenName\":\"Max\",\
           \\"deathday\":null,\
           \\"patronymic\":null,\
           \\"children\":[],\
           \\"birthday\":null,\
           \\"parents\":{\"father\":null,\"mother\":null},\
           \\"id\":1,\
           \\"surname\":\"Planck\"}]"
          {matchStatus = 200}

addTestPerson :: Sql.Connection -> IO ()
addTestPerson connection = do
    Sql.execute_ connection
      "INSERT INTO person (givenName, surname, parents, children) VALUES \
      \('Max', 'Planck', '{\"motherId\":2,\"fatherId\":3}', '[]')"
    Sql.execute_ connection
      "INSERT INTO person (id, givenName, surname, parents, children) VALUES \
      \(3, 'Johann', 'Planck', '', '[{\"spouseId\":2, \"childrenIds\":[1]}]')"

