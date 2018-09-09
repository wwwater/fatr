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



createSchema :: Connection -> IO ()
createSchema conn = do
  executeDB "CREATE TABLE IF NOT EXISTS person\
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


selectPersonById :: Connection -> Int -> IO (Maybe M.PersonDB)
selectPersonById conn personId = do
  result <- (query conn "SELECT id, givenName, surname, patronymic, birthday, deathday, parents, children FROM person WHERE id = ?"
            (Only personId) :: IO [M.PersonDB])
  case (length result) of
      0 -> return Nothing
      _ -> return $ Just $ head result

selectAboutPersonById :: Connection -> Int -> IO (Maybe M.AboutPersonDB)
selectAboutPersonById conn personId = do
  result <- (query conn "SELECT id, givenName, surname, patronymic, birthday, deathday, about FROM person WHERE id = ?"
            (Only personId) :: IO [M.AboutPersonDB])
  case (length result) of
      0 -> return Nothing
      _ -> return $ Just $ head result

search :: Connection -> String -> IO ([M.PersonDB])
search conn searchString = do
    let searchWords = words searchString
        firstWord = if length searchWords > 0 then searchWords !! 0 else ""
        secondWord = if length searchWords > 1 then searchWords !! 1 else "" in
      queryNamed conn
        "SELECT id, givenName, surname, patronymic, birthday, deathday, parents, children FROM person WHERE \
        \(surname LIKE :w1 AND givenName LIKE :w2) \
        \OR (surname LIKE :w2 AND givenName LIKE :w1) LIMIT 10"
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

