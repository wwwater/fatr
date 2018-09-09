module CommonHtml       exposing (..)

import Html             exposing (..)
import Html.Attributes  exposing ( style
                                 , id
                                 , class
                                 , src
                                 , title
                                 )
import Html.Events      exposing ( on
                                 , keyCode
                                 , onClick
                                 )

import Json.Decode      as Json
import Char             exposing (fromCode)

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

drawPersonBox : Maybe Person -> List (Attribute msg) -> (Int -> msg) -> Html msg
drawPersonBox maybePerson personStyles action =
  div ([ onClick (action <| Maybe.withDefault 0 <| Maybe.map .id maybePerson)
      , title "Информация" ] ++ personStyles)
      [ drawBarePerson maybePerson ]



treeBackground : Html msg
treeBackground =
  div [ style [ ("display", "flex")
          , ("width", "100%")
          , ("min-width", "1000px")
          , ("z-index", "-3")
          , ("justify-content", "center")
          , ("margin-left", "auto")
          , ("margin-right", "auto")
          ] ]
      [ img [ src "/assets/background.svg"
            , style [ ("max-height", "90%")
                    , ("position", "absolute")
                    , ("top", "10%")
                    , ("opacity", "0.6")
                    ]
            ]
            []
      ]

formatDate : String -> String
formatDate d =
  String.join "." <| List.reverse <| String.split "-" d


formatDates : Person -> String
formatDates person =
    let birth = formatDate <| Maybe.withDefault "?" person.birthday
        death = Maybe.withDefault "" <| Maybe.map (\d ->
          String.fromChar (Char.fromCode 0xA0)  -- nbsp
          ++ "| " ++ (formatDate d)) person.deathday in
        birth ++ death

displayAsSearchEntry : Person -> String
displayAsSearchEntry person =
    (Maybe.withDefault "?" person.surname) ++ " " ++
    (Maybe.withDefault "" person.givenName) ++ " " ++
    (Maybe.withDefault "" <| Maybe.map (\b -> String.left 4 b) person.birthday)


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
  on "keyup" (Json.map tagger keyCode)
