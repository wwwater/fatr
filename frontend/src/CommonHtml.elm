module CommonHtml       exposing (..)

import Html             exposing (..)
import Html.Attributes  exposing ( style, id, class )
import Html.Events      exposing ( on, keyCode )

import Json.Decode      as Json

import Styles           exposing (..)
import ServerApi        exposing ( Person )

drawBarePerson : Maybe Person -> Html msg
drawBarePerson maybePerson =
    case maybePerson of
        Just person ->
          div [ personBareStyle
              , id <| "person-" ++ toString person.id
              ]
                [ div [] [ text <| Maybe.withDefault "" <| person.surname ]
                , div [] [ text <| Maybe.withDefault "" <| person.givenName ]
                , div [] [ text <| formatDates person ]
                , div [ id <| "incoming-line-" ++ toString person.id
                      , class "incoming-line"
                      , style [ ("height", "2px")
                              , ("background-color", "#666")
                              , ("position", "absolute")
                              , ("cursor" ,"auto")
                              , ("z-index", "-2")
                              ]
                      ]
                      []

                ]
        Nothing -> div [] [ text "?" ]

formatDates : Person -> String
formatDates person =
    let birth = Maybe.withDefault "?" person.birthday
        death = Maybe.withDefault "" <| Maybe.map (\d -> " | " ++ d) person.deathday in
        birth ++ death

displayAsSearchEntry : Person -> String
displayAsSearchEntry person =
    (Maybe.withDefault "?" person.surname) ++ " " ++
    (Maybe.withDefault "" person.givenName) ++ " " ++
    (Maybe.withDefault "" <| Maybe.map (\b -> String.left 4 b) person.birthday)


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
  on "keyup" (Json.map tagger keyCode)
