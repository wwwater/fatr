module PersonTreeTest exposing (..)

import Test                 exposing (..)
import Fuzz                 exposing (string)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (text, tag, class)
import Expect

import PersonTree
import TestUtils            exposing (..)
import ServerApi            exposing (Person)

import PersonInfoDialog


all : Test
all =
    describe "PersonTree component"
        [ describe "testing HTML"
            [ test "no person found" <|
                \() ->
                    PersonTree.view (testModel Nothing)
                    |> Query.fromHtml
                    |> Query.findAll [ class "person" ]
                    |> Query.count (Expect.equal 0)
            , test "person without relatives displayed" <|
                \() ->
                    PersonTree.view (testModel <| Just testPerson)
                    |> Query.fromHtml
                    |> Query.findAll [ class "person" ]
                    |> Query.count (Expect.equal 1)
            , test "person tree displayed" <|
                \() ->
                    PersonTree.view (testModel <| Just testPersonWithAll)
                    |> Query.fromHtml
                    |> Query.findAll [ class "person" ]
                    |> Query.count (Expect.equal 7)
            ]
        ]

testModel : Maybe Person -> PersonTree.Model
testModel maybePerson  = PersonTree.Model maybePerson [] PersonInfoDialog.init Nothing
