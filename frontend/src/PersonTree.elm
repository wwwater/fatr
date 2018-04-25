module PersonTree       exposing ( Model
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
  = HandlePersonRetrieved (Result Http.Error Person)
  | GoToPersonTree Int


init : Model
init =
  Model Nothing Nothing


mountCmd : Int -> Jwt -> Cmd Msg
mountCmd personId jwt =
  ServerApi.getPersonTree personId jwt HandlePersonRetrieved


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    HandlePersonRetrieved res ->
      case res of
        Result.Ok person ->
          onlyUpdateModel { model | person = Just person }

        Result.Err err ->
          handleServerError { model | person = Nothing } err
    GoToPersonTree id -> ( model, Routes.navigate (Routes.PersonTreePage id) )



drawChild : Person -> Html Msg
drawChild child =
  div [ personWithOthersStyle
      , class "person"
      ]
      [ drawDescendants child
      , div [ personBoxStyle child.birthday
            , onClick (GoToPersonTree child.id)
            , title "Построить древо" ]
            [ drawBarePerson (Just child) ]

      ]

drawDescendants : Person -> Html Msg
drawDescendants person =
  let
    drawChildrenWithSpouse : Children -> Html Msg
    drawChildrenWithSpouse childrenWithSpouse =
      let spouse = getSpouse childrenWithSpouse
          children = getChildren childrenWithSpouse in
          div [ personWithOthersStyle
              , class "children-with-spouse"
              ]
              [ if List.length children > 0 then
                  div [ branchesStyle
                      , style [ ("border", "2px dotted #333") ]
                      ]
                      (List.map drawChild children)
                else div [] []
              , div [ spouseStyle
                    , onClick (GoToPersonTree <| Maybe.withDefault 0 <| Maybe.map .id spouse)
                    , title "Построить древо" ]
                    [ drawBarePerson spouse ]
              ]
  in
    if List.length person.children > 0 then
      div [ branchesStyle ]
          (List.map drawChildrenWithSpouse
          <| List.reverse person.children)
    else div [] []

drawAncestor : Maybe Person -> Html Msg
drawAncestor maybePerson =
  case maybePerson of
    Just person ->
      div [ personWithOthersStyle
          , class "person"
          ]
          [ div [ personBoxStyle person.birthday
                , onClick (GoToPersonTree person.id)
                , title "Построить древо"
                ]
                [ drawBarePerson maybePerson ]
          , drawAncestors person
          ]
    Nothing -> div [] []

drawAncestors : Person -> Html Msg
drawAncestors person =
  let maybeFather = getFather person
      maybeMother = getMother person in
    if maybeFather /= Nothing || maybeMother /= Nothing then
      div [ branchesStyle
          , style [ ("border", "2px dotted #333")
                  , ("align-items", "start")
                  ]
          ]
          [ drawAncestor (getFather person)
          , drawAncestor (getMother person)
          ]
    else div [] []



view : Model -> Html Msg
view model =
  div [ pageStyle ]
      [ case model.error of
        Just error -> h2 [ ] [ text error ]
        Nothing ->
          case model.person of
            Just person ->
              div [ treePageContentStyle ]
                  [ div [ personWithOthersStyle
                        , style [ ("margin-right", "0") ]
                        ]
                        [ drawDescendants person ]
                  , div [ personBoxStyle person.birthday
                        , style [ ("border", "3px dotted #eee") ]
                        , class "person"
                        ]
                        [ drawBarePerson model.person ]
                  , drawAncestors person
                  ]
            Nothing -> div [] []
      ]
