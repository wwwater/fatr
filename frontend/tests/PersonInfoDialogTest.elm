module PersonInfoDialogTest exposing (..)

import Test                 exposing (..)
import Fuzz                 exposing (string)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (text, tag, class)
import Expect

import PersonInfoDialog
import TestUtils            exposing (..)
import ServerApi            exposing (Person)



all : Test
all =
    describe "PersonInfoDialog component"
        [ describe "testing HTML"
            [ test "no dialog shown" <|
                \() ->
                    PersonInfoDialog.view (testModel False)
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.count (Expect.equal 0)
            , test "dialog is shown" <|
                \() ->
                    PersonInfoDialog.view (testModel True)
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.count (Expect.equal 3)
            ]
        ]

testModel : Bool -> PersonInfoDialog.Model
testModel show  = PersonInfoDialog.Model (Just testPerson) show Nothing
