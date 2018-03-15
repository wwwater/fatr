module MenuTest exposing (..)

import Test                 exposing (..)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (tag, class)

import Menu


all : Test
all =
    describe "Menu component"
        [ describe "testing HTML"
            [ test "menu has home button" <|
                \() ->
                    Menu.view
                    |> Query.fromHtml
                    |> Query.has [ tag "div" ]
            ]
        ]

