
module Logic where

import Crypto.PasswordStore                 (verifyPassword)
import Data.ByteString.Char8                (pack)
import Data.Char                            (toLower)
import Data.Maybe                           (fromMaybe)
import Data.Text                            (unpack)
import Database.SQLite.Simple               (Connection)
import System.Environment                   (lookupEnv)


import Jwt                                  (createJwt, verifyJwt)
import Model
import qualified Storage as S


defaultJwtSecret :: String
defaultJwtSecret = "jwt-secret"


issueJwt :: Connection -> Credentials -> IO (Maybe Jwt)
issueJwt conn credentials =
  let user = username credentials
      passwordProvided = map toLower $ password credentials in do
  maybePasswordHash <- S.getUserPassword conn user
  case maybePasswordHash of
    Just passwordHash ->
      if verifyPassword (pack passwordProvided) (pack passwordHash)
        then do
          jwtSecret <- lookupEnv "JWT_SECRET"
          let secret = fromMaybe defaultJwtSecret jwtSecret in
            fmap (Just . Jwt . unpack) $ createJwt secret
        else return Nothing
    Nothing -> return Nothing

verifyJwtToken :: JwtToken -> IO Bool
verifyJwtToken jwtToken = do
  jwtSecret <- lookupEnv "JWT_SECRET"
  let secret = fromMaybe defaultJwtSecret jwtSecret
  verifyJwt secret jwtToken



getPersonById :: Connection -> Int -> IO (Maybe Person)
getPersonById conn personId = do
  maybePersonDB <- S.selectPersonById conn personId
  return $ fmap toPerson maybePersonDB

search :: Connection -> String -> IO [Person]
search conn searchString = do
  persons <- S.search conn searchString
  return $ fmap toPerson persons

getAncestors :: Connection -> Int -> IO (Maybe Person)
getAncestors conn personId = do
  maybePersonDB <- S.selectPersonById conn personId
  case maybePersonDB of
    Just personDB ->
      let person = toPerson personDB
          maybeMotherId = (motherId . parentsDB) personDB
          maybeFatherId = (fatherId . parentsDB) personDB
          maybeMotherIO = case maybeMotherId of
              Just mId -> getAncestors conn mId
              Nothing -> return Nothing
          maybeFatherIO = case maybeFatherId of
              Just fId -> getAncestors conn fId
              Nothing -> return Nothing
          in do
            maybeMother <- maybeMotherIO
            maybeFather <- maybeFatherIO
            let withMother = addMother person maybeMother
                withFather = addFather withMother maybeFather
            return $ Just $ withFather
    Nothing -> return Nothing

getDescendants :: Connection -> Int -> IO (Maybe Person)
getDescendants conn personId = do
  maybePersonDB <- S.selectPersonById conn personId
  case maybePersonDB of
    Just personDB ->
      let person = toPerson personDB
          childrenListDB = (\(ChildrenDB cDB) -> cDB) $ childrenDB personDB
          in foldl (\acc x -> getAndAddChildrenWithSpouse conn acc x) (return $ Just person) childrenListDB
    Nothing -> return Nothing

getPersonTree :: Connection -> Int -> IO (Maybe Person)
getPersonTree conn personId = do
  maybePersonDB <- S.selectPersonById conn personId
  case maybePersonDB of
    Just personDB ->
      let person = toPerson personDB
          maybeMotherId = (motherId . parentsDB) personDB
          maybeFatherId = (fatherId . parentsDB) personDB
          maybeMotherIO = case maybeMotherId of
              Just mId -> getAncestors conn mId
              Nothing -> return Nothing
          maybeFatherIO = case maybeFatherId of
              Just fId -> getAncestors conn fId
              Nothing -> return Nothing
          in do
            maybeMother <- maybeMotherIO
            maybeFather <- maybeFatherIO
            let withMother = addMother person maybeMother
                withFather = addFather withMother maybeFather
                childrenListDB = (\(ChildrenDB cDB) -> cDB) $ childrenDB personDB
                in foldl (\p cs -> getAndAddChildrenWithSpouse conn p cs) (return $ Just withFather) childrenListDB
    Nothing -> return Nothing

getAndAddChildrenWithSpouse :: Connection -> IO (Maybe Person) -> ChildrenWithSpouseDB -> IO (Maybe Person)
getAndAddChildrenWithSpouse conn maybePersonIO childrenWithSpouseDB =
  let maybeSpouseIO = case spouseId childrenWithSpouseDB of
        Just sId -> getPersonById conn sId
        Nothing -> return Nothing
      childrenWithSpouseIds = childrenIds childrenWithSpouseDB
      childrenIO = foldl getAndAddChild (return []) childrenWithSpouseIds
    in do
        maybeSpouse <- maybeSpouseIO
        theirChildren <- childrenIO
        maybePerson <- maybePersonIO
        return $ fmap (\p -> addChildrenWithSpouse p maybeSpouse theirChildren) maybePerson
  where
    getAndAddChild :: IO ([Person]) -> Int -> IO ([Person])
    getAndAddChild childrenListIO childId = do
      maybeChild <- getDescendants conn childId
      childrenList <- childrenListIO
      case maybeChild of
        Just child -> return $ child:childrenList
        Nothing -> childrenListIO


toPerson :: PersonDB -> Person
toPerson personDB =
  Person {
           Model.id = idDB personDB,
           givenName = givenNameDB personDB,
           surname = surnameDB personDB,
           patronymic = patronymicDB personDB,
           birthday = birthdayDB personDB,
           deathday = deathdayDB personDB,
           parents = Parents Nothing Nothing,
           children = Children []
           }

addMother :: Person -> Maybe Person -> Person
addMother person maybeMother =
  person { parents = Parents maybeMother ((father . parents) person) }

addFather :: Person -> Maybe Person -> Person
addFather person maybeFather =
  person { parents = Parents ((mother . parents) person) maybeFather }

addChildrenWithSpouse :: Person -> Maybe Person -> [Person] -> Person
addChildrenWithSpouse person maybeSpouse theirChildren =
  let childrenWithOneMoreSpouse = ChildrenWithSpouse maybeSpouse theirChildren
      childrenWithOtherSpousesList = (\(Children cs) -> cs) $ children person
      in
      person { children = Children $ childrenWithOneMoreSpouse:childrenWithOtherSpousesList }

