module Routes exposing (..)

import UrlParser exposing (Parser, (</>), int, oneOf, s)
import Navigation exposing (Location)
import Html.Attributes exposing (href, attribute)
import Html exposing (Html, Attribute, a)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json


type Route
    = TreePage
    | AncestorsPage Int


routeParser : Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map TreePage (s "")
        , UrlParser.map AncestorsPage (s "person" </> int </> s "ancestors")
        ]


decode : Location -> Maybe Route
decode location =
    UrlParser.parsePath routeParser location



encode : Route -> String
encode route =
    case route of

        TreePage ->
            "/"

        AncestorsPage i ->
            "person/" ++ toString i ++ "/ancestors"



navigate : Route -> Cmd msg
navigate route =
    Navigation.newUrl (encode route)

