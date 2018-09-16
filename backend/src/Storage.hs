{-# LANGUAGE OverloadedStrings #-}
module Storage where


import Database.SQLite.Simple   (Connection,
                                query,
                                queryNamed,
                                Only (..),
                                execute_,
                                NamedParam (..)
                                )
import Data.Char                (toUpper)


import qualified Model          as M

{--
From Database.SQLite.Simple documentation:
> newtype Query
> A query string. This type is intended to make it difficult to construct a SQL query by concatenating string fragments,
> as that is an extremely common way to accidentally introduce SQL injection vulnerabilities into an application.

In combination with the fact that SQLite3 doesn't allow parameter substitution for table name,
we have the situation, when there must be a separate query string for each table name
--}

createSchema :: Connection -> IO ()
createSchema conn = do
  executeDB "CREATE TABLE IF NOT EXISTS irkutsk_person\
            \(id INTEGER PRIMARY KEY ASC,\
            \givenName VARCHAR2(255),\
            \surname VARCHAR2(255),\
            \patronymic VARCHAR2(255),\
            \birthday VARCHAR2(31),\
            \deathday VARCHAR2(31),\
            \parents VARCHAR2(255),\
            \children VARCHAR2(255),\
            \about TEXT)"
  executeDB "CREATE TABLE IF NOT EXISTS user \
            \(name VARCHAR2(255) PRIMARY KEY, password TEXT)"
  where
    executeDB = execute_ conn


selectPersonById :: Connection -> String -> Int -> IO (Maybe M.PersonDB)
selectPersonById conn user personId = do
  let queryWithTableName = case user of
        "irkutsk" -> "SELECT id, givenName, surname, patronymic, birthday, deathday, parents, children FROM irkutsk_person WHERE id = ?"
        _ -> error ("Table person for user " ++ user ++ " does not exist")
  result <- (query conn queryWithTableName (Only personId) :: IO [M.PersonDB])
  case (length result) of
      0 -> return Nothing
      _ -> return $ Just $ head result

selectAboutPersonById :: Connection -> String -> Int -> IO (Maybe M.AboutPersonDB)
selectAboutPersonById conn user personId = do
  let queryWithTableName = case user of
        "irkutsk" -> "SELECT id, givenName, surname, patronymic, birthday, deathday, about FROM irkutsk_person WHERE id = ?"
        _ -> error ("Table person for user " ++ user ++ " does not exist")
  result <- (query conn queryWithTableName (Only personId) :: IO [M.AboutPersonDB])
  case (length result) of
      0 -> return Nothing
      _ -> return $ Just $ head result

search :: Connection -> String -> String -> IO ([M.PersonDB])
search conn user searchString = do
    let queryWithTableName = case user of
          "irkutsk" -> "SELECT id, givenName, surname, patronymic, birthday, deathday, parents, children FROM irkutsk_person WHERE \
            \(surname LIKE :w1 AND givenName LIKE :w2) \
            \OR (surname LIKE :w2 AND givenName LIKE :w1) LIMIT 10"
          _ -> error ("Table person for user " ++ user ++ " does not exist")
        searchWords = words searchString
        firstWord = if length searchWords > 0 then searchWords !! 0 else ""
        secondWord = if length searchWords > 1 then searchWords !! 1 else "" in
      queryNamed conn queryWithTableName
        [
          ":w1" := (capitalize firstWord) ++ "%",
          ":w2" := (capitalize secondWord) ++ "%"
        ]
        :: IO [M.PersonDB]


capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = (toUpper x):xs


getUserPassword :: Connection -> String -> IO (Maybe String)
getUserPassword conn username = do
  result <- (query conn "SELECT name, password FROM user WHERE name = ?"
            (Only username) :: IO [M.Credentials])
  case (length result) of
      0 -> return Nothing
      _ -> return $ Just $ M.password $ head result

