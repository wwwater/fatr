module Ancestors        exposing ( Model
                                 , Msg
                                 , init
                                 , view
                                 , update
                                 , mountCmd
                                 )

import Html             exposing (..)
import Html.Attributes  exposing ( style, class, title )
import Html.Events      exposing ( onClick )
import Http

import Global           exposing ( handleServerError
                                 , onlyUpdateModel
                                 )
import ServerApi        exposing (..)
import Routes
import Styles           exposing (..)
import CommonHtml       exposing (..)



type alias Model =
    { person : Maybe Person
    , error : Maybe String
    }


type Msg
    = HandleAncestorsRetrieved (Result Http.Error Person)
    | GoToDescendants Int


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
                    onlyUpdateModel { model | person = Just person }

                Result.Err err ->
                    handleServerError { model | person = Nothing } err
        GoToDescendants id -> ( model, Routes.navigate (Routes.DescendantsPage id) )


drawPerson : Maybe Person -> Int -> Html Msg
drawPerson maybePerson depth =
    case maybePerson of
        Just person ->
            div [ personWithOthersStyle
                , style [ ("justify-content", "start") ]
                , class "person"
                ]
                [ div [ personBoxStyle depth
                      , onClick (GoToDescendants person.id)
                      , title "Построить древо потомков" ]
                      [ drawBarePerson maybePerson ]
                , div [ branchesStyle, style [ ("border-top", "1px dotted #333") ]]
                      [ drawPerson (getFather person) (depth + 1)
                      , drawPerson (getMother person) (depth + 1) ]
                ]
        Nothing -> div [] []

view : Model -> Html Msg
view model =
    div [ treePageStyle ]
        [ case model.error of
            Just error -> h2 [ ] [ text error ]
            Nothing ->
                case model.person of
                    Just person ->
                        div [ treePageContentStyle ]
                            [ h2 [ style [ ("display", "flex")
                                         , ("margin-bottom", "20px")
                                         ] ]
                                 [ text <| (getNameAndPatronymic person) ++ " и Их древо предков" ]
                            , drawPerson model.person 0
                            ]
                    Nothing -> div [] []
        ]
