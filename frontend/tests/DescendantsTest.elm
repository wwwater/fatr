module DescendantsTest      exposing (..)

import Test                 exposing (..)
import Fuzz                 exposing (string)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (text, tag, class)
import Expect

import Descendants
import TestUtils            exposing (..)
import ServerApi            exposing (Person)


all : Test
all =
    describe "Descendants component"
        [ describe "testing HTML"
            [ test "no person found" <|
                \() ->
                    Descendants.view (testModel Nothing)
                    |> Query.fromHtml
                    |> Query.findAll [ class "person" ]
                    |> Query.count (Expect.equal 0)
            , test "person without descendants displayed" <|
                \() ->
                    Descendants.view (testModel <| Just testPerson)
                    |> Query.fromHtml
                    |> Query.findAll [ class "person" ]
                    |> Query.count (Expect.equal 1)
            , test "descendants tree displayed" <|
                \() ->
                    Descendants.view (testModel <| Just testPersonWithDescendants)
                    |> Query.fromHtml
                    |> Query.findAll [ class "person" ]
                    |> Query.count (Expect.equal 3)
            ]
        ]

testModel : Maybe Person -> Descendants.Model
testModel maybePerson  = Descendants.Model maybePerson Nothing
