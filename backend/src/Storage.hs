{-# LANGUAGE OverloadedStrings #-}
module Storage where


import Database.SQLite.Simple   (Connection,
                                query,
                                Only (..),
                                execute_)

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
            \children VARCHAR2(255))"

  where
    executeDB = execute_ conn


selectPersonById :: Connection -> Int -> IO (Maybe M.PersonDB)
selectPersonById conn personId = do
  result <- (query conn "SELECT givenName, surname, patronymic, birthday, deathday, parents, children FROM person WHERE id = ?"
            (Only personId) :: IO [M.PersonDB])
  case (length result) of
      0 -> return Nothing
      _ -> return $ Just $ head result

