module Routes exposing (..)

import UrlParser exposing (Parser, (</>), int, oneOf, s)
import Navigation exposing (Location)
import Html.Attributes exposing (href, attribute)
import Html exposing (Html, Attribute, a)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json


type Route
    = AncestorsPage Int
    | DescendantsPage Int
    | LoginPage


routeParser : Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map AncestorsPage (s "person" </> int </> s "ancestors")
        , UrlParser.map DescendantsPage (s "person" </> int </> s "descendants")
        , UrlParser.map LoginPage (s "login")
        , UrlParser.map LoginPage (s "")
        ]


decode : Location -> Maybe Route
decode location =
    UrlParser.parsePath routeParser location



encode : Route -> String
encode route =
    case route of

        AncestorsPage i ->
            "/person/" ++ toString i ++ "/ancestors"

        DescendantsPage i ->
            "/person/" ++ toString i ++ "/descendants"

        LoginPage ->
            "/login"


navigate : Route -> Cmd msg
navigate route =
    Navigation.newUrl (encode route)

