module Descendants      exposing ( Model
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
    = HandleDescendantsRetrieved (Result Http.Error Person)
    | GoToAncestors Int


init : Model
init =
    Model Nothing Nothing


mountCmd : Int -> Jwt -> Cmd Msg
mountCmd personId jwt =
    ServerApi.getDescendants personId jwt HandleDescendantsRetrieved


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        HandleDescendantsRetrieved res ->
            case res of
                Result.Ok person ->
                    onlyUpdateModel { model | person = Just person }

                Result.Err err ->
                    handleServerError { model | person = Nothing } err
        GoToAncestors id -> ( model, Routes.navigate (Routes.AncestorsPage id) )



drawChildrenWithSpouse : Children -> Int -> Html Msg
drawChildrenWithSpouse childrenWithSpouse depth =
    let spouse = getSpouse childrenWithSpouse
        children = getChildren childrenWithSpouse in
            div [ personWithOthersStyle
                , style [ ("justify-content", "flex-end") ]
                , class "children-with-spouse"
                ]
                [ div [ branchesStyle
                      , style [ ("border-bottom", "1px dotted #333") ]
                      ]
                      (List.map (\c -> drawPerson (Just c) (depth - 1)) children)
                , div [ spouseStyle
                      , onClick (GoToAncestors <| Maybe.withDefault 0 <| Maybe.map .id spouse)
                      , title "Построить древо предков" ]
                      [ drawBarePerson spouse ]
                ]


drawPerson : Maybe Person -> Int -> Html Msg
drawPerson maybePerson depth =
    case maybePerson of
        Just person ->
            div [ personWithOthersStyle
                , style [ ("justify-content", "flex-end") ]
                , class "person"
                ]
                [ div [ branchesStyle ]
                      (List.map (\c -> drawChildrenWithSpouse c depth)
                      <| List.reverse person.children)
                , div [ personBoxStyle depth
                      , onClick (GoToAncestors person.id)
                      , title "Построить древо предков" ]
                      [ drawBarePerson maybePerson ]
                ]
        Nothing -> div [] []

view : Model -> Html Msg
view model =
    div [ pageStyle ]
        [ case model.error of
            Just error -> h2 [ ] [ text error ]
            Nothing ->
                case model.person of
                    Just person ->
                        div [ treePageContentStyle ] [
                            h2 [ style [ ("display", "flex")
                                       , ("margin-bottom", "20px")
                                       ] ]
                               [ text <| (getNameAndPatronymic person) ++ " и потомки" ]
                            , drawPerson model.person -1
                            ]
                    Nothing -> div [] []
        ]


