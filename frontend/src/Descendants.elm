module Descendants      exposing ( Model
                                 , Msg
                                 , init
                                 , view
                                 , update
                                 , mountCmd
                                 )

import Html             exposing (..)
import Html.Attributes  exposing ( style, class )
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
    = HandleDescendantsRetrieved (Result Http.Error Person)


init : Model
init =
    Model Nothing Nothing


mountCmd : Int -> Cmd Msg
mountCmd personId =
    ServerApi.getDescendants personId HandleDescendantsRetrieved


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        HandleDescendantsRetrieved res ->
            case res of
                Result.Ok person ->
                    onlyUpdateModel { model | person = Just person }

                Result.Err err ->
                    handleServerError { model | person = Nothing } err




drawChildrenWithSpouse : Children -> Int -> Html Msg
drawChildrenWithSpouse childrenWithSpouse depth =
    let spouse = getSpouse childrenWithSpouse
        children = getChildren childrenWithSpouse in
            div [ personWithOthersStyle
                , style [ ("justify-content", "end") ]
                , class "children-with-spouse"
                ]
                [ div [ branchesStyle
                      , style [ ("border-bottom", "1px dotted #333") ]
                      ]
                      (List.map (\c -> drawPerson (Just c) (depth - 1)) children)
                , div [ spouseStyle ]
                      [ drawBarePerson spouse ]
                ]


drawPerson : Maybe Person -> Int -> Html Msg
drawPerson maybePerson depth =
    case maybePerson of
        Just person ->
            div [ personWithOthersStyle
                , style [ ("justify-content", "end") ]
                , class "person"
                ]
                [ div [ branchesStyle ]
                      (List.map (\c -> drawChildrenWithSpouse c depth)
                      <| List.reverse person.children)
                , div [ personBoxStyle depth ]
                      [ drawBarePerson maybePerson ]
                ]
        Nothing -> div [] []

view : Model -> Html Msg
view model =
    div [ treePageStyle ]
        [ case model.error of
            Just error -> h2 [ ] [ text error ]
            Nothing -> drawPerson model.person -1
        ]
