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

data ChildrenWithSpouse = ChildrenWithSpouse
  { spouse :: Maybe Person
  , childrenWithSpouse :: [Person]
  } deriving (Generic)

newtype Children = Children [ChildrenWithSpouse]
    deriving (Generic)

data Person = Person
  { id :: Int
  , givenName :: Maybe String
  , surname :: Maybe String
  , patronymic :: Maybe String
  , birthday :: Maybe String
  , deathday :: Maybe String
  , parents :: Parents
  , children :: Children
  , photo :: Maybe String
  , about :: Maybe String
  } deriving (Generic)




data ParentsDB = ParentsDB
  { motherId :: Maybe Int
  , fatherId :: Maybe Int
  } deriving (Generic)

data ChildrenWithSpouseDB = ChildrenWithSpouseDB
  { spouseId :: Maybe Int
  , childrenIds :: [Int]
  } deriving (Generic)

newtype ChildrenDB = ChildrenDB [ChildrenWithSpouseDB]
    deriving (Generic)

data PersonDB = PersonDB
  { idDB :: Int
  , givenNameDB :: Maybe String
  , surnameDB :: Maybe String
  , patronymicDB :: Maybe String
  , birthdayDB :: Maybe String
  , deathdayDB :: Maybe String
  , parentsDB :: ParentsDB
  , childrenDB :: ChildrenDB
  } deriving (Generic)

data AboutPersonDB = AboutPersonDB
  { idAboutDB :: Int
  , givenNameAboutDB :: Maybe String
  , surnameAboutDB :: Maybe String
  , patronymicAboutDB :: Maybe String
  , birthdayAboutDB :: Maybe String
  , deathdayAboutDB :: Maybe String
  , photoDB :: Maybe String
  , aboutDB :: Maybe String
  } deriving (Generic)


data Credentials = Credentials
  { username :: String
  , password :: String
  } deriving (Generic)

type JwtSecret = String
type JwtToken = String

data Jwt = Jwt
  { token :: JwtToken
  } deriving (Generic)

instance FromRow PersonDB where
  fromRow = PersonDB <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow AboutPersonDB where
  fromRow = AboutPersonDB <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

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


instance FromRow Credentials where
  fromRow = Credentials <$> field <*> field

instance ToJSON ParentsDB
instance FromJSON ParentsDB
instance ToJSON ChildrenDB
instance FromJSON ChildrenDB
instance ToJSON ChildrenWithSpouseDB
instance FromJSON ChildrenWithSpouseDB

instance ToJSON Person
instance ToJSON Parents
instance ToJSON Children
instance ToJSON ChildrenWithSpouse


instance FromJSON Credentials
instance ToJSON Credentials
instance FromJSON Jwt
instance ToJSON Jwt

