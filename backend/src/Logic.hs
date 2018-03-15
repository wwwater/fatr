
module Logic where

import Database.SQLite.Simple               (Connection)


import Model
import qualified Storage as S


getPersonById :: Connection -> Int -> IO (Maybe Person)
getPersonById conn personId = do
    maybePersonDB <- S.selectPersonById conn personId
    return $ fmap toPerson maybePersonDB

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


toPerson :: PersonDB -> Person
toPerson personDB =
    Person {
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
