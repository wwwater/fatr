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
         \\"about\":\"Great physicist\",\
         \\"id\":1,\
         \\"surname\":\"Planck\"}"
        {matchStatus = 200}

  describe "test GET /person/{id}/tree endpoint" $
    after (\connection -> do
          Sql.execute_ connection "DROP TABLE person"
          Sql.execute_ connection "DROP TABLE user"
          ) $ do

    it "returns unauthorized without jwt" $ \connection ->
      addUserAndCheck connection $
        request "GET"
                "/person/1/tree"
                []
                ""
        `shouldRespondWith` 401

    it "retrieves person's tree" $ \connection ->
      setupAndCheck connection addTestPerson $ do
        response <- makeJwtRequest
        request "GET"
                "/person/1/tree"
                [("Content-Type", "application/json"), ("jwt", getJwtFromResponse response)]
                ""
        `shouldRespondWith`
          "{\"givenName\":\"Max\",\
           \\"deathday\":null,\
           \\"patronymic\":null,\
           \\"children\":[{\"spouse\":null,\
                          \\"childrenWithSpouse\":[\
                                  \{\"givenName\":\"Erwinn\",\
                                   \\"deathday\":null,\
                                   \\"patronymic\":null,\
                                   \\"children\":[],\
                                   \\"birthday\":null,\
                                   \\"parents\":\
                                    \{\"father\":null,\
                                     \\"mother\":null},\
                                   \\"about\":null,\
                                   \\"id\":5,\
                                   \\"surname\":\"Planck\"}]}],\
           \\"birthday\":null,\
           \\"parents\":\
            \{\"father\":{\"givenName\":\"Johann\",\
                         \\"deathday\":null,\
                         \\"patronymic\":null,\
                         \\"children\":[],\
                         \\"birthday\":null,\
                         \\"parents\":{\"father\":null,\"mother\":null},\
                         \\"about\":null,\
                         \\"id\":3,\
                         \\"surname\":\"Planck\"},\
             \\"mother\":null},\
           \\"about\":null,\
           \\"id\":1,\
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
           \\"about\":null,\
           \\"id\":1,\
           \\"surname\":\"Planck\"}]"
          {matchStatus = 200}

  describe "test GET /person/{id}/siblings endpoint" $
    after (\connection -> do
          Sql.execute_ connection "DROP TABLE person"
          Sql.execute_ connection "DROP TABLE user"
          ) $ do

    it "returns unauthorized without jwt" $ \connection ->
      addUserAndCheck connection $
        request "GET"
                "/person/1/siblings"
                []
                ""
        `shouldRespondWith` 401

    it "retrieves person's tree" $ \connection ->
      setupAndCheck connection addTestPerson $ do
        response <- makeJwtRequest
        request "GET"
                "/person/5/siblings"
                [("Content-Type", "application/json"), ("jwt", getJwtFromResponse response)]
                ""
        `shouldRespondWith`
          "[[{\"givenName\":\"Erwinn\",\
           \\"deathday\":null,\
           \\"patronymic\":null,\
           \\"children\":[],\
           \\"birthday\":null,\
           \\"parents\":{\"father\":null,\"mother\":null},\
           \\"about\":null,\
           \\"id\":5,\
           \\"surname\":\"Planck\"}],\
           \[],\
           \[{\"givenName\":\"CousinOfErwinn\",\
           \\"deathday\":null,\
           \\"patronymic\":null,\
           \\"children\":[],\
           \\"birthday\":null,\
           \\"parents\":{\"father\":null,\"mother\":null},\
           \\"about\":null,\
           \\"id\":7,\
           \\"surname\":\"Planck\"}],[]]"
          {matchStatus = 200}

addTestPerson :: Sql.Connection -> IO ()
addTestPerson connection = do
    Sql.execute_ connection
      "INSERT INTO person (id, givenName, surname, parents, children, about) VALUES \
      \(1, 'Max', 'Planck', '{\"motherId\":2,\"fatherId\":3}', '[{\"spouseId\":4, \"childrenIds\":[5]}]', 'Great physicist')"
    Sql.execute_ connection
      "INSERT INTO person (id, givenName, surname, parents, children) VALUES \
      \(3, 'Johann', 'Planck', '', '[{\"spouseId\":2, \"childrenIds\":[1,6]}]')"
    Sql.execute_ connection
      "INSERT INTO person (id, givenName, surname, parents, children) VALUES \
      \(5, 'Erwinn', 'Planck', '{\"motherId\":4,\"fatherId\":1}', '[]')"
    Sql.execute_ connection
      "INSERT INTO person (id, givenName, surname, parents, children) VALUES \
      \(6, 'BrotherOfMax', 'Planck', '', '[{\"spouseId\":null, \"childrenIds\":[7]}]')"
    Sql.execute_ connection
      "INSERT INTO person (id, givenName, surname, parents, children) VALUES \
      \(7, 'CousinOfErwinn', 'Planck', '', '[]')"

