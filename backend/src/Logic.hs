
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

getPersonTree :: Connection -> Int -> IO (Maybe Person)
getPersonTree conn personId = do
    withAncestors <- getAncestors conn personId
    withDescendants <- getDescendants conn personId
    return $ mergePerson withAncestors withDescendants


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
                withParents = addFather withMother maybeFather
            return $ Just withParents
    Nothing -> return Nothing

getDescendants :: Connection -> Int -> IO (Maybe Person)
getDescendants conn personId = do
  maybePersonDB <- S.selectPersonById conn personId
  case maybePersonDB of
    Just personDB -> do
      let person = toPerson personDB
          childrenListDB = (\(ChildrenDB cDB) -> cDB) $ childrenDB personDB
      withChildren <- foldl getAndAddChildrenWithSpouse (return person) childrenListDB
      return $ Just withChildren
    Nothing -> return Nothing

  where

    getAndAddChildrenWithSpouse :: IO Person -> ChildrenWithSpouseDB -> IO Person
    getAndAddChildrenWithSpouse personIO childrenWithSpouseDB =
      let maybeSpouseIO = case spouseId childrenWithSpouseDB of
            Just sId -> getPersonById conn sId
            Nothing -> return Nothing
          childrenWithSpouseIds = childrenIds childrenWithSpouseDB
          childrenIO = foldl getAndAddChild (return []) childrenWithSpouseIds
        in do
            maybeSpouse <- maybeSpouseIO
            theirChildren <- childrenIO
            person <- personIO
            return $ addChildrenWithSpouse person maybeSpouse theirChildren

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

mergePerson :: Maybe Person -> Maybe Person -> Maybe Person
mergePerson maybePersonWithAncestors maybePersonWithDescendants =
  case maybePersonWithAncestors of
    Just withAncestors ->
      case maybePersonWithDescendants of
        Just withDescendants ->
          Just $ withAncestors { children = children withDescendants }
        Nothing -> maybePersonWithAncestors
    Nothing -> maybePersonWithDescendants

