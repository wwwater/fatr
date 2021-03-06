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
            , test "dialog has img" <|
                \() ->
                    PersonInfoDialog.view (testModel True)
                    |> Query.fromHtml
                    |> Query.findAll [ tag "img" ]
                    |> Query.count (Expect.equal 1)
            , test "dialog has no img tag if no photo" <|
                \() ->
                    PersonInfoDialog.view
                      (PersonInfoDialog.Model (Just testPersonWithAll) True "" Nothing)
                    |> Query.fromHtml
                    |> Query.findAll [ tag "img" ]
                    |> Query.count (Expect.equal 0)
            , test "dialog has about-person text" <|
                \() ->
                    PersonInfoDialog.view (testModel True)
                    |> Query.fromHtml
                    |> Query.has
                        [ text <| Maybe.withDefault "" <| testPerson.about]
            ]
        ]

testModel : Bool -> PersonInfoDialog.Model
testModel show  = PersonInfoDialog.Model (Just testPerson) show "2019-03-29" Nothing
