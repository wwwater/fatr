module PersonTree       exposing ( Model
                                 , Msg
                                 , init
                                 , view
                                 , update
                                 , mountCmd
                                 )

import Html             exposing (..)
import Html.Attributes  exposing ( style, class, title, id )
import Http

import Global           exposing ( handleServerError
                                 , onlyUpdateModel
                                 )
import ServerApi        exposing (..)
import Styles           exposing (..)
import CommonHtml       exposing (..)
import ConnectionsUtil  exposing (..)

import PersonInfoDialog



type alias Model =
  { person : Maybe Person
  , connections : List (Int, Int)
  , personInfoDialogModel : PersonInfoDialog.Model
  , error : Maybe String
  }


type Msg
  = HandlePersonRetrieved (Result Http.Error Person)
  | PersonInfoDialogMsg PersonInfoDialog.Msg
  | ShowPersonInfo Int


init : String -> Model
init initDate =
  Model Nothing [] (PersonInfoDialog.init initDate) Nothing


mountCmd : Int -> Jwt -> Cmd Msg
mountCmd personId jwt =
  ServerApi.getPersonTree personId jwt HandlePersonRetrieved


update : Msg -> Jwt -> Model -> ( Model, Cmd Msg )
update action jwt model =
  case action of
    HandlePersonRetrieved res ->
      case res of
        Result.Ok person ->
          onlyUpdateModel { model | person = Just person
                                  , connections = calculateConnectingLines person }

        Result.Err err ->
          handleServerError { model | person = Nothing, connections = [] } err

    PersonInfoDialogMsg m ->
      let ( subMdl, subCmd ) = PersonInfoDialog.update m model.personInfoDialogModel
        in { model | personInfoDialogModel = subMdl } !
            [ Cmd.map PersonInfoDialogMsg subCmd ]

    ShowPersonInfo personId ->
      model ! [ Cmd.map PersonInfoDialogMsg <| PersonInfoDialog.mountCmd personId jwt ]






drawChild : Person -> Html Msg
drawChild child =
  div [ personWithOthersStyle
      , class "person"
      ]
      [ drawDescendants child
      , drawPersonBox (Just child) [personBoxStyle child.birthday] ShowPersonInfo
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
              [ div [ branchesStyle ]
                    (List.map drawChild children)
              , drawPersonBox spouse [spouseStyle] ShowPersonInfo
              ]
  in
    div [ branchesStyle ]
        (List.map drawChildrenWithSpouse
        <| List.reverse person.children)

drawAncestor : Maybe Person -> Html Msg
drawAncestor maybePerson =
  case maybePerson of
    Just person ->
      div [ personWithOthersStyle
          , class "person"
          ]
          [ drawPersonBox maybePerson [personBoxStyle person.birthday] ShowPersonInfo
          , drawAncestors person
          ]
    Nothing -> div [] []

drawAncestors : Person -> Html Msg
drawAncestors person =
  let maybeFather = getFather person
      maybeMother = getMother person in
    div [ branchesStyle
        , style [ ("align-items", "start") ]
        ]
        [ drawAncestor (getFather person)
        , drawAncestor (getMother person)
        ]


drawTree : Maybe Person -> Html Msg
drawTree maybePerson =
  case maybePerson of
    Just person ->
      div [ treeStyle
          , id "tree-page-content"
          , style [ ("opacity", "0") ]
          ]
          [ div [ personWithOthersStyle
                , style [ ("margin-right", "0") ]
                ]
                [ drawDescendants person ]
          , drawPersonBox
              maybePerson
              [ personBoxStyle person.birthday
              , style [ ("border", "3px dotted #eee") ]
              , class "person"
              ]
              ShowPersonInfo
          , drawAncestors person
          ]
    Nothing -> div [] []


view : Model -> Html Msg
view model =
  div [ pageStyle ]
      [ treeBackground
      , case model.error of
          Just error -> h2 [ style [ ("align-self", "center") ] ] [ text error ]
          Nothing -> div [] []
      , h2 [ style [ ("align-self", "center") ] ] [ text "Древо прямых предков и потомков" ]
      , div [ style [("flex-grow", "3") ] ] []
      , drawTree model.person
      , div [ style [("flex-grow", "1") ] ] []
      , Html.map PersonInfoDialogMsg <| PersonInfoDialog.view model.personInfoDialogModel
      ]
