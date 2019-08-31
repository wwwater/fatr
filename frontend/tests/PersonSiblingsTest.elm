module PersonSiblingsTest exposing (..)

import Test                 exposing (..)
import Fuzz                 exposing (string)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (text, tag, class)
import Expect

import PersonSiblings
import TestUtils            exposing (..)
import ServerApi            exposing (Person)

import PersonInfoDialog


all : Test
all =
    describe "PersonSiblings component"
        [ describe "testing HTML"
            [ test "draws siblings" <|
                \() ->
                    PersonSiblings.view testModel
                    |> Query.fromHtml
                    |> Query.findAll [ class "person" ]
                    |> Query.count (Expect.equal 3)
            , test "do not draw titles of empty groups" <|
                \() ->
                    PersonSiblings.view testModel
                    |> Query.fromHtml
                    |> Query.findAll [ tag "h4" ]
                    |> Query.count (Expect.equal 2)
            ]
        ]

testModel : PersonSiblings.Model
testModel =
  PersonSiblings.Model
    [[testPerson], [], [testPerson, testPerson], []]
    (PersonInfoDialog.init "2019-03-29")
    Nothing
