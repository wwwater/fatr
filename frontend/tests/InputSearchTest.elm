module InputSearchTest exposing (..)

import Test                 exposing (..)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (id, class)
import Expect

import TestUtils            exposing (..)

import InputSearch

type Msg =
      OnSearch String
    | OnClick
    | NoOp


all : Test
all =
    describe "InputSearch component"
        [ describe "testing HTML"
            [ test "has input" <|
                \() ->
                    InputSearch.view (withOptions [])
                    |> Query.fromHtml
                    |> Query.findAll [ id "search-option-0" ]
                    |> Query.count (Expect.equal 1)
            , test "has no options dropdown" <|
                \() ->
                    InputSearch.view (withOptions [])
                    |> Query.fromHtml
                    |> Query.findAll [ id "search-option-1" ]
                    |> Query.count (Expect.equal 0)
            , test "has an option in dropdown" <|
                \() ->
                    InputSearch.view (withOptions [InputSearch.Option "option" OnClick])
                    |> Query.fromHtml
                    |> Query.findAll [ id "search-option-1" ]
                    |> Query.count (Expect.equal 1)
            ]
        ]

withOptions : List (InputSearch.Option Msg) -> InputSearch.Model Msg
withOptions options = InputSearch.Model options "placeholder" OnSearch "" 0 NoOp
