
module Logic (issueJwt,
             verifyJwtToken,
             getAboutPersonById,
             search,
             getPersonTree,
             findSiblings
             ) where

import Crypto.PasswordStore                 (verifyPassword)
import Data.ByteString.Char8                (pack)
import Data.Char                            (toLower)
import Data.List                            (notElem, nub)
import Data.Maybe                           (fromMaybe, fromJust, isJust)
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

getAboutPersonById :: Connection -> Int -> IO (Maybe Person)
getAboutPersonById conn personId = do
  maybeAboutPersonDB <- S.selectAboutPersonById conn personId
  return $ fmap toPersonWithAbout maybeAboutPersonDB

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


-- function returns siblings of a person grouped by kinship, e.g.
-- [[1st kinship siblings, i.e. brothers and sisters], [cousins], [2nd cousins], ...]
-- function consists of 3 parts:
-- 1. find all paths of all descendants of the persons deepest ancestors
-- 2. divide all these siblings into groups by kinship
-- 3. convert siblings ids to persons
findSiblings :: Connection -> Int -> IO [[Person]]
findSiblings conn pId = do
  siblingsPaths <- collectSameLevelSiblings pId 0 []
  let maxDepth = maximum $ map length siblingsPaths
  let siblingsIds = [pId] : map (\d -> filterSiblingsOfKinship siblingsPaths d) [1 .. maxDepth]
  sequence $ map (\siblingsOfKinship -> do
                 siblings <- sequence $ map (\s -> getPersonById conn s) siblingsOfKinship
                 return $ map fromJust $ filter isJust siblings
                 )
                 siblingsIds

  where

    -- traverse the tree to collect all siblings of the same level
    -- (same level means cousins but not uncles).
    -- first ascend the tree until the earliest ancestors
    -- then descend all branches until reached the initial depth
    -- prepend path upon descending to form the following paths
    -- [
    -- [personId, personMotherId, personMaternalGrandmothedId],
    -- [personId, personMotherId, personMaternalGrandFatherId],
    -- [personId, personFatherId, personPaternalGrandMotherId],
    -- ...
    -- [cousinId, cousinMothedId, personMatenalGrandmotherId],
    -- ...
    -- ]
    collectSameLevelSiblings :: Int -> Int -> [Int] -> IO [[Int]]
    collectSameLevelSiblings personId depth path = do
      maybePersonDB <- S.selectPersonById conn personId
      case maybePersonDB of
        Nothing -> return [path]
        Just personDB ->
          let maybeMotherId = (motherId . parentsDB) personDB
              maybeFatherId = (fatherId . parentsDB) personDB in
            if null path && (maybeMotherId /= Nothing || maybeFatherId /= Nothing)  -- ascending
              then
                let siblingsFromMotherSideIO =
                      case maybeMotherId of
                        Nothing -> return []
                        Just mId -> collectSameLevelSiblings mId (depth + 1) path
                    siblingsFromFatherSideIO =
                      case maybeFatherId of
                        Nothing -> return []
                        Just fId -> collectSameLevelSiblings fId (depth + 1) path

                  in do
                        siblingsFromMotherSide <- siblingsFromMotherSideIO
                        siblingsFromFatherSide <- siblingsFromFatherSideIO
                        return $ siblingsFromMotherSide ++ siblingsFromFatherSide
              else
                if depth == 0
                  then return [personId : path] -- reached sibling of the same level, returning it
                  else -- descending
                    let allChildrenIds = concat $ map childrenIds $ (\(ChildrenDB cDB) -> cDB) $ childrenDB personDB in
                      if null allChildrenIds
                        then return [] -- branch has no siblings of the same level, nothing returned
                        else fmap concat $
                             sequence $
                             map (\childId -> collectSameLevelSiblings childId (depth - 1) (personId : path)) allChildrenIds

    -- among all siblings filter out those that have the same (kinship)-th ancestor but different ones up to it
    -- e.g. for kinship = 2 all cousins will be selected, which means all
    -- siblings that have the same number on the index 2, but different index 1
    -- (i.e. different person, with different parents but with the same grandparent)
    filterSiblingsOfKinship :: [[Int]] -> Int -> [Int]
    filterSiblingsOfKinship paths kinship =
      let personPaths = filter (\p -> length p >= (kinship + 1) && head p == pId) paths
          personUniqPathElements = nub $ map (\p -> p !! (kinship - 1)) personPaths -- not-matching previous path element
          matchingAncestors = nub $ map (\p -> p !! kinship) personPaths -- matching ancestors
          siblingsOfKinshipPaths = filter (\p -> length p >= (kinship + 1) &&
                 let notMatchingPathElement = p !! (kinship - 1)
                     matchingAncestor = p !! (kinship) in
                       elem matchingAncestor matchingAncestors &&
                       notElem notMatchingPathElement personUniqPathElements) paths
        in nub $ map head siblingsOfKinshipPaths










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
           children = Children [],
           about = Nothing
           }

toPersonWithAbout :: AboutPersonDB -> Person
toPersonWithAbout aboutPersonDB =
  Person {
           Model.id = idAboutDB aboutPersonDB,
           givenName = givenNameAboutDB aboutPersonDB,
           surname = surnameAboutDB aboutPersonDB,
           patronymic = patronymicAboutDB aboutPersonDB,
           birthday = birthdayAboutDB aboutPersonDB,
           deathday = deathdayAboutDB aboutPersonDB,
           parents = Parents Nothing Nothing,
           children = Children [],
           about = aboutDB aboutPersonDB
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

