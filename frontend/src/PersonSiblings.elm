module PersonSiblings   exposing ( Model
                                 , Msg
                                 , init
                                 , view
                                 , update
                                 , mountCmd
                                 )

import Html             exposing (..)
import Html.Attributes  exposing ( style, class, title, id )
import Html.Events      exposing ( onClick )
import Http

import Global           exposing ( handleServerError
                                 , onlyUpdateModel
                                 , nthElement
                                 )
import ServerApi        exposing (..)
import Routes
import Styles           exposing (..)
import CommonHtml       exposing (..)
import ConnectionsUtil  exposing (..)



type alias Model =
  { siblings : List (List Person)
  , error : Maybe String
  }


type Msg
  = HandleSiblingsRetrieved (Result Http.Error (List (List Person)))
  | GoToPersonSiblings Int


init : Model
init =
  Model [] Nothing


mountCmd : Int -> Jwt -> Cmd Msg
mountCmd personId jwt =
  ServerApi.getPersonSiblings personId jwt HandleSiblingsRetrieved


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    HandleSiblingsRetrieved res ->
      case res of
        Result.Ok siblings ->
          onlyUpdateModel { model | siblings = siblings }

        Result.Err err ->
          handleServerError { model | siblings = [] } err
    GoToPersonSiblings id ->
      ( model , Routes.navigate (Routes.PersonSiblingsPage id) )


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
  div [ personBoxStyle person.birthday
      , onClick (GoToPersonSiblings person.id)
      , title "Построить диаграмму братьев и сестер"
      , class "person"
      ]
      [ drawBarePerson (Just person) ]

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

view : Model -> Html Msg
view model =
  div [ pageStyle
      , style [ ("justify-content", "center") ]
      ]
      [ case model.error of
          Just error -> h2 [ ] [ text error ]
          Nothing -> div [] []
      , h2 [ style [ ("align-self", "center") ] ] [ text "Братья и сестры" ]
      , div [ style [ ("flex-grow", "1") ] ] []
      , div [ style [ ("display", "flex")
                    , ("flex-direction", "row")
                    ]
            ]
            (List.map drawSiblingsOfKinship
            <| List.map2 (,) (List.range 0 (List.length model.siblings)) model.siblings)
      , div [ style [ ("flex-grow", "1") ] ] []
      ]
