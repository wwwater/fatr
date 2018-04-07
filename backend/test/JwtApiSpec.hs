{-# LANGUAGE OverloadedStrings #-}


module JwtApiSpec (main, spec) where

import Test.Hspec               (hspec, Spec, beforeAll, afterAll, after, describe, it)
import Test.Hspec.Wai           (request, shouldRespondWith)

import qualified Database.SQLite.Simple as Sql

import Util



main :: IO ()
main = hspec spec


testConnect :: IO Sql.Connection
testConnect = Sql.open ":memory:"


spec :: Spec
spec = beforeAll testConnect $
       afterAll Sql.close $ do

  describe "test POST /jwt endpoint" $
    after (\connection -> Sql.execute_ connection "DROP TABLE user") $ do

    it "retrieves a JWT" $ \connection ->
      addUserAndCheck connection $
        request "POST"
                "/jwt"
                [("Content-Type", "application/json")]
                (createCredentialsJson "test" "testPassword")
            `shouldRespondWith` 200

    it "returns an error if user does not exist" $ \connection ->
      addUserAndCheck connection $
        request "POST"
                "/jwt"
                [("Content-Type", "application/json")]
                (createCredentialsJson "not-existent-user" "testPassword")
            `shouldRespondWith` 401

    it "returns an error if password is wrong" $ \connection ->
      addUserAndCheck connection $
        request "POST"
                "/jwt"
                [("Content-Type", "application/json")]
                (createCredentialsJson "test" "wrongPassword")
            `shouldRespondWith` 401

