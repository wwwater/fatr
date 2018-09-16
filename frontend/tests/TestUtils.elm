module TestUtils exposing (..)

import ServerApi exposing (..)

testPerson : Person
testPerson = Person
    1
    (Just "Max")
    (Just "Planck")
    (Nothing)
    (Just "1858-04-23")
    (Just "1947-10-4")
    (Parents { mother = Nothing, father = Nothing })
    []
    (Just "/assets/photo/max.png")
    (Just "Hello there")

testParents : Parents
testParents =
    (\m f -> Parents { mother = m, father = f })
    (Just { testPerson | id = 2
                       , parents = Parents { mother = Just { testPerson | id = 22 }
                                           , father = Nothing } })
    (Just { testPerson | id = 3 })

testChildren : Children
testChildren =
    (\s c -> Children { spouse = s, childrenWithSpouse = c })
    (Just { testPerson | id = 4
                       , parents = Parents { father = Just { testPerson | id = 43 }
                                           , mother = Nothing } })
    [ { testPerson | id = 5 }
    , { testPerson | id = 6
                   , children = [ Children { spouse = Nothing
                                         , childrenWithSpouse = [ { testPerson | id = 65} ] } ] }
    ]

testPersonWithAll : Person
testPersonWithAll = Person
    1
    (Just "Max")
    (Just "Planck")
    Nothing
    (Just "1858-04-23")
    (Just "1947-10-4")
    testParents
    [testChildren]
    Nothing
    Nothing
