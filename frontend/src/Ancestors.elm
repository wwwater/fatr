module Ancestors        exposing ( Model
                                 , Msg
                                 , init
                                 , view
                                 , update
                                 , mountCmd
                                 )

import Html             exposing (..)
import Html.Attributes  exposing ( style)
import Html.Events      exposing ( onClick )
import Http

import Global           exposing ( handleServerError
                                 , onlyUpdateModel
                                 )
import ServerApi        exposing (..)
import Routes



type alias Model =
    { person : Maybe Person
    , error : Maybe String
    }


type Msg
    = HandleAncestorsRetrieved (Result Http.Error Person)


init : Model
init =
    Model Nothing Nothing


mountCmd : Int -> Cmd Msg
mountCmd personId =
    ServerApi.getAncestors personId HandleAncestorsRetrieved


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        HandleAncestorsRetrieved res ->
            case res of
                Result.Ok person ->
                    let _ = Debug.log "" person in
                    onlyUpdateModel { model | person = Just person }

                Result.Err err ->
                    handleServerError { model | person = Nothing } err

getMother : Parents -> Maybe Person
getMother (Parents parents) =
    parents.mother

getFather : Parents -> Maybe Person
getFather (Parents parents) =
    parents.father

formatDates : Person -> String
formatDates person =
    let birth = Maybe.withDefault "?" person.birthday
        death = Maybe.withDefault "" <| Maybe.map (\d -> " | " ++ d) person.deathday in
        birth ++ death

drawPerson : Maybe Person -> Html Msg
drawPerson maybePerson =
    case maybePerson of
        Just person ->
            div [ style [ ("display", "flex")
                        , ("flex-direction", "column")
                        , ("justify-content", "end")
                        , ("align-items", "center")
                        ] ]
                [ div [ style [ ("display", "flex")
                              , ("flex-direction", "row")
                              , ("justify-content", "center")
                              ] ]
                      [ drawPerson <| getMother person.parents
                      , drawPerson <| getFather person.parents ]
                , div [ style [ ("display", "flex")
                              , ("padding", "10px")
                              , ("margin", "10px")
                              , ("border", "1px solid #333")
                              , ("border-radius", "20px")
                              , ("background-color", "#ddd")
                              , ("box-shadow", "2px 2px 15px 1px #333")
                              ] ]
                      [ div [ style [ ("display", "flex")
                                    , ("flex-direction", "column")
                                    , ("align-items", "center")
                                    ] ]
                            [ div [] [ text <| Maybe.withDefault "" <| person.surname ]
                            , div [] [ text <| Maybe.withDefault "" <| person.givenName ]
                            , div [] [ text <| formatDates person ]
                            ] ]
                ]
        Nothing -> div [] []

view : Model -> Html Msg
view model =
    div [ style [ ("background-color", "#aaa")
                , ("display", "flex")
                , ("justify-content", "center")
                , ("flex-grow", "1")
                , ("font-size", "18px")
                , ("font-weight", "bold")
                ] ]
        [ case model.error of
            Just error ->
                h2 [ ] [ text error ]
            Nothing -> drawPerson model.person
        ]
