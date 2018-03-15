{-# LANGUAGE DeriveGeneric #-}

module Model where

import GHC.Generics                         (Generic)


import Database.SQLite.Simple               (FromRow (..),
                                            field,
                                            SQLData( SQLText ),
                                            ResultError( ConversionFailed ))
import Database.SQLite.Simple.FromField     (FromField (..),
                                            returnError)
import Database.SQLite.Simple.Internal      (Field (..))
import Database.SQLite.Simple.Ok            (Ok (..) )

import Data.Aeson                           (FromJSON,
                                            ToJSON,
                                            decode)

import Data.Text                            (unpack)
import Data.ByteString.Lazy.Char8           (pack)


data Parents = Parents
  { mother :: Maybe Person
  , father :: Maybe Person
  } deriving (Generic)

data ChildrenWithOneSpouse = ChildrenWithOneSpouse
  { spouse :: Maybe Person
  , childrenWithSpouse :: [Person]
  } deriving (Generic)

newtype Children = Children [ChildrenWithOneSpouse]
    deriving (Generic)

data Person = Person
  { givenName :: Maybe String
  , surname :: Maybe String
  , patronymic :: Maybe String
  , birthday :: Maybe String
  , deathday :: Maybe String
  , parents :: Parents
  , children :: Children
  } deriving (Generic)




data ParentsDB = ParentsDB
  { motherId :: Maybe Int
  , fatherId :: Maybe Int
  } deriving (Generic)

data ChildrenWithOneSpouseDB = ChildrenWithOneSpouseDB
  { spouseId :: Maybe Int
  , childrenIds :: [Int]
  } deriving (Generic)

newtype ChildrenDB = ChildrenDB [ChildrenWithOneSpouseDB]
    deriving (Generic)

data PersonDB = PersonDB
  { givenNameDB :: Maybe String
  , surnameDB :: Maybe String
  , patronymicDB :: Maybe String
  , birthdayDB :: Maybe String
  , deathdayDB :: Maybe String
  , parentsDB :: ParentsDB
  , childrenDB :: ChildrenDB
  } deriving (Generic)

instance FromRow PersonDB where
  fromRow = PersonDB <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromField ParentsDB where
  fromField (Field (SQLText str) _) = Ok $
    case (decode . pack . unpack) str of
      Nothing -> ParentsDB Nothing Nothing
      Just ps -> ps
  fromField f = returnError ConversionFailed f "cannot convert to Parents"

instance FromField ChildrenDB where
  fromField (Field (SQLText str) _) = Ok $
    case (decode . pack . unpack) str of
      Just (ChildrenDB cs) -> ChildrenDB cs
      _ -> ChildrenDB []
  fromField f = returnError ConversionFailed f "cannot convert to Children"

instance ToJSON PersonDB
instance ToJSON ParentsDB
instance FromJSON ParentsDB
instance ToJSON ChildrenDB
instance FromJSON ChildrenDB
instance ToJSON ChildrenWithOneSpouseDB
instance FromJSON ChildrenWithOneSpouseDB

instance ToJSON Person
instance ToJSON Parents
instance ToJSON Children
instance ToJSON ChildrenWithOneSpouse
