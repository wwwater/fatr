module TestUtils exposing (..)

import ServerApi exposing (..)

testPerson : Person
testPerson = Person
    (Just "Max")
    (Just "Planck")
    (Nothing)
    (Just "1858-04-23")
    (Just "1947-10-4")
    (Parents { mother = Nothing, father = Nothing })
    []

testParents : Parents
testParents =
    (\m f -> Parents { mother = m, father = f })
    (Just testPerson)
    (Just testPerson)


testPersonWithAncestors : Person
testPersonWithAncestors = Person
    (Just "Max")
    (Just "Planck")
    Nothing
    (Just "1858-04-23")
    (Just "1947-10-4")
    testParents
    []

testChildren : Children
testChildren =
    (\s c -> Children { spouse = s, childrenWithSpouse = c })
    (Just testPerson)
    [testPerson, testPerson]

testPersonWithDescendants : Person
testPersonWithDescendants = Person
    (Just "Max")
    (Just "Planck")
    Nothing
    (Just "1858-04-23")
    (Just "1947-10-4")
    (Parents { mother = Nothing, father = Nothing })
    [testChildren]
