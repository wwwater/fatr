module TestUtils exposing (..)

import ServerApi

testPerson : ServerApi.Person
testPerson = ServerApi.Person
    (Just "Max")
    (Just "Planck")
    (Nothing)
    (Just "1858-04-23")
    (Just "1947-10-4")
    (ServerApi.Parents { mother = Nothing, father = Nothing })
    []

testParents : ServerApi.Parents
testParents =
    (\m f -> ServerApi.Parents { mother = m, father = f })
    (Just testPerson)
    (Just testPerson)


testPersonWithAncestors : ServerApi.Person
testPersonWithAncestors = ServerApi.Person
    (Just "Max")
    (Just "Planck")
    Nothing
    (Just "1858-04-23")
    (Just "1947-10-4")
    testParents
    []
