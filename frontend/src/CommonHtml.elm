module CommonHtml   exposing (..)

import Html         exposing (..)

import Styles       exposing (..)
import ServerApi    exposing ( Person )


drawBarePerson : Maybe Person -> Html msg
drawBarePerson maybePerson =
    case maybePerson of
        Just person ->
          div [ personBareStyle ]
                [ div [] [ text <| Maybe.withDefault "" <| person.surname ]
                , div [] [ text <| Maybe.withDefault "" <| person.givenName ]
                , div [] [ text <| formatDates person ]
                ]
        Nothing -> div [] [ text "?" ]

formatDates : Person -> String
formatDates person =
    let birth = Maybe.withDefault "?" person.birthday
        death = Maybe.withDefault "" <| Maybe.map (\d -> " | " ++ d) person.deathday in
        birth ++ death


getNameAndPatronymic : Person -> String
getNameAndPatronymic person =
    (Maybe.withDefault "?" person.givenName) ++ " " ++
    (Maybe.withDefault "" person.patronymic)

displayAsSearchEntry : Person -> String
displayAsSearchEntry person =
    (Maybe.withDefault "?" person.surname) ++ " " ++
    (Maybe.withDefault "" person.givenName) ++ " " ++
    (Maybe.withDefault "" <| Maybe.map (\b -> String.left 4 b) person.birthday)
