module RoutesTest exposing (..)

import Test         exposing (..)
import Expect

import Navigation   exposing (Location)
import Routes       exposing (encode)


all : Test
all =
    describe "testing Routes"
        [ describe "encoding routes"
            [ test "/person/{id}/ancestors" <|
                \() -> Routes.encode (Routes.AncestorsPage 1)
                |> Expect.equal "person/1/ancestors"
            ]

        , describe "decoding routes"
            [ test "/person/{id}/ancestors" <|
                \() -> Routes.decode (Location "" "" "" "" "" "" "/person/1/ancestors" "" "" "" "")
                |> Expect.equal (Just (Routes.AncestorsPage 1))
            , test "/not-existing-path" <|
                \() -> Routes.decode (Location "" "" "" "" "" "" "/not-exist" "" "" "" "")
                |> Expect.equal Nothing
            ]
        ]
