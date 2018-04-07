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
                |> Expect.equal "/person/1/ancestors"
            , test "/person/{id}/descendants" <|
                \() -> Routes.encode (Routes.DescendantsPage 1)
                |> Expect.equal "/person/1/descendants"
            , test "/login" <|
                \() -> Routes.encode (Routes.LoginPage)
                |> Expect.equal "/login"
            ]

        , describe "decoding routes"
            [ test "/person/{id}/ancestors" <|
                \() -> Routes.decode (Location "" "" "" "" "" "" "/person/1/ancestors" "" "" "" "")
                |> Expect.equal (Just (Routes.AncestorsPage 1))
            , test "/person/{id}/descendants" <|
                \() -> Routes.decode (Location "" "" "" "" "" "" "/person/1/descendants" "" "" "" "")
                |> Expect.equal (Just (Routes.DescendantsPage 1))
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
