module PersonSiblings   exposing ( Model
                                 , Msg
                                 , init
                                 , view
                                 , update
                                 , mountCmd
                                 )

import Html             exposing (..)
import Html.Attributes  exposing ( style, class, title, id, src )
import Http

import Global           exposing ( handleServerError
                                 , onlyUpdateModel
                                 , nthElement
                                 )
import ServerApi        exposing (..)
import Styles           exposing (..)
import CommonHtml       exposing (..)

import PersonInfoDialog


type alias Model =
  { siblings : List (List Person)
  , personInfoDialogModel : PersonInfoDialog.Model
  , error : Maybe String
  }


type Msg
  = HandleSiblingsRetrieved (Result Http.Error (List (List Person)))
  | PersonInfoDialogMsg PersonInfoDialog.Msg
  | ShowPersonInfo Int


init : Model
init =
  Model [] PersonInfoDialog.init Nothing


mountCmd : Int -> Jwt -> Cmd Msg
mountCmd personId jwt =
  ServerApi.getPersonSiblings personId jwt HandleSiblingsRetrieved


update : Msg -> Jwt -> Model -> ( Model, Cmd Msg )
update action jwt model =
  case action of
    HandleSiblingsRetrieved res ->
      case res of
        Result.Ok siblings ->
          onlyUpdateModel { model | siblings = siblings }

        Result.Err err ->
          handleServerError { model | siblings = [] } err

    PersonInfoDialogMsg m ->
      let ( subMdl, subCmd ) = PersonInfoDialog.update m model.personInfoDialogModel
        in { model | personInfoDialogModel = subMdl } !
            [ Cmd.map PersonInfoDialogMsg subCmd ]

    ShowPersonInfo personId ->
      model ! [ Cmd.map PersonInfoDialogMsg <| PersonInfoDialog.mountCmd personId jwt ]



siblingTitles : List String
siblingTitles =
  [ "."
  , "Родные"
  , "Двоюродные"
  , "Троюродные"
  , "Четвероюродные"
  , "Пятиюродные"
  ]

drawPerson : Person -> Html Msg
drawPerson person =
  drawPersonBox
    (Just person)
    [personBoxStyle person.birthday
    , class "person"
    ]
    ShowPersonInfo

drawSiblings : List Person -> Html Msg
drawSiblings siblings =
  div [ style [ ("display", "flex")
              , ("flex-wrap", "wrap")
              , ("align-items", "center")
              ]
      ]
      (List.map drawPerson
      <| List.reverse
      <| List.sortWith comparatorByAge siblings)


drawSiblingsOfKinship : (Int, List Person) -> Html Msg
drawSiblingsOfKinship (kinship, siblings) =
  if List.length siblings > 0
  then
    div [ style [ ("display", "flex")
                , ("flex-direction", "column")
                , ("align-items", "center")
                , ("margin", "20px")
                ]
        ]
        [ h4 [] [text <| Maybe.withDefault "" <| nthElement siblingTitles kinship]
        , drawSiblings siblings
        ]
  else div [] []


background : Html msg
background =
  img [ src "/assets/siblings4.2.svg"
      , style [ ("max-width", "100%")
              , ("opacity", "0.8")
              , ("position", "absolute")
              , ("z-index", "-3")
              ]
      ]
      []

view : Model -> Html Msg
view model =
  div [ pageStyle ]
      [
      case model.error of
          Just error -> h2 [ ] [ text error ]
          Nothing -> div [] []
      , h2 [ style [ ("align-self", "center") ] ] [ text "Братья и сестры" ]
      , div [ style [ ("display", "flex")
                    , ("flex-direction", "row")
                    , ("max-width", "1200px")
                    , ("position", "relative")
                    ]
            ]
            (background::(
              List.map drawSiblingsOfKinship
            <| List.map2 (,) (List.range 0 (List.length model.siblings))
            model.siblings))
      , Html.map PersonInfoDialogMsg <| PersonInfoDialog.view model.personInfoDialogModel
      ]
