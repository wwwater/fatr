module AncestorsTest exposing (..)

import Test                 exposing (..)
import Fuzz                 exposing (string)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (text, tag, class)
import Expect

import Ancestors
import TestUtils            exposing (..)
import ServerApi            exposing (Person)


all : Test
all =
    describe "Ancestors component"
        [ describe "testing HTML"
            [ test "no person found" <|
                \() ->
                    Ancestors.view (testModel Nothing)
                    |> Query.fromHtml
                    |> Query.findAll [ class "person" ]
                    |> Query.count (Expect.equal 0)
            , test "person without ancestors displayed" <|
                \() ->
                    Ancestors.view (testModel <| Just testPerson)
                    |> Query.fromHtml
                    |> Query.findAll [ class "person" ]
                    |> Query.count (Expect.equal 1)
            , test "ancestors tree displayed" <|
                \() ->
                    Ancestors.view (testModel <| Just testPersonWithAncestors)
                    |> Query.fromHtml
                    |> Query.findAll [ class "person" ]
                    |> Query.count (Expect.equal 3)
            ]
        ]

testModel : Maybe Person -> Ancestors.Model
testModel maybePerson  = Ancestors.Model maybePerson Nothing
