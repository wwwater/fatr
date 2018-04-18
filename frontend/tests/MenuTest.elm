module MenuTest exposing (..)

import Test                 exposing (..)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (id, class)
import Expect

import TestUtils            exposing (..)

import Menu


all : Test
all =
    describe "Menu component"
        [ describe "testing HTML"
            [ test "menu has input" <|
                \() ->
                    Menu.view Menu.init
                    |> Query.fromHtml
                    |> Query.findAll [ id "search-option-0" ]
                    |> Query.count (Expect.equal 1)
            , test "menu has no options dropdown" <|
                \() ->
                    Menu.view Menu.init
                    |> Query.fromHtml
                    |> Query.findAll [ id "search-option-1" ]
                    |> Query.count (Expect.equal 0)
            ]
        ]


