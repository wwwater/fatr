module RoutesTest exposing (..)

import Test         exposing (..)
import Expect

import Navigation   exposing (Location)
import Routes       exposing (encode)


all : Test
all =
    describe "testing Routes"
        [ describe "encoding routes"
            [ test "/person/{id}/tree" <|
                \() -> Routes.encode (Routes.PersonTreePage 1)
                |> Expect.equal "/person/1/tree"
            , test "/login" <|
                \() -> Routes.encode (Routes.LoginPage)
                |> Expect.equal "/login"
            ]

        , describe "decoding routes"
            [ test "/person/{id}/tree" <|
                \() -> Routes.decode (Location "" "" "" "" "" "" "/person/1/tree" "" "" "" "")
                |> Expect.equal (Just (Routes.PersonTreePage 1))
            , test "/login" <|
                \() -> Routes.decode (Location "" "" "" "" "" "" "/login" "" "" "" "")
                |> Expect.equal (Just (Routes.LoginPage))
            , test "/" <|
                \() -> Routes.decode (Location "" "" "" "" "" "" "/" "" "" "" "")
                |> Expect.equal (Just (Routes.LoginPage))
            , test "/not-existing-path" <|
                \() -> Routes.decode (Location "" "" "" "" "" "" "/not-exist" "" "" "" "")
                |> Expect.equal Nothing
            ]
        ]
