module PersonInfoDialog exposing (Model, Msg, init, view, update, mountCmd)

import Html             exposing (..)
import Html.Attributes  exposing ( style
                                 , class
                                 )
import Html.Events      exposing ( onClick )
import Http

import Dialog

import Global           exposing ( handleServerError
                                 , onlyUpdateModel
                                 )
import ServerApi        exposing (..)
import CommonHtml       exposing (..)
import Routes           exposing (..)


type alias Model =
    { person : Maybe Person
    , show : Bool
    , error : Maybe String
    }


type Msg
    = HandlePersonRetrieved (Result Http.Error Person)
    | Close
    | GoToPersonTree Int
    | GoToPersonSiblings Int


init : Model
init = Model Nothing False Nothing

mountCmd : Int -> Jwt -> Cmd Msg
mountCmd personId jwt =
  ServerApi.getPersonTree personId jwt HandlePersonRetrieved


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    HandlePersonRetrieved res ->
      case res of
        Result.Ok person ->
          onlyUpdateModel { model | person = Just person, show = True }
        Result.Err err ->
          handleServerError { model | person = Nothing } err

    Close ->
      onlyUpdateModel { model | show = False }

    GoToPersonTree id ->
      ( model, Routes.navigate (Routes.PersonTreePage id) )

    GoToPersonSiblings id ->
      ( model, Routes.navigate (Routes.PersonSiblingsPage id) )


drawPersonName : Person -> Html Msg
drawPersonName person =
  let surname = Maybe.withDefault "?" <| person.surname
      givenName = Maybe.withDefault "?" <| person.givenName
      patronymic = Maybe.withDefault "" <| person.patronymic
      displayName = surname ++ " " ++ givenName ++ " " ++ patronymic
  in h3 [] [text displayName]

drawPersonInfo : Person -> Html Msg
drawPersonInfo person =
  text <| formatDates person

drawButtons : Person -> Html Msg
drawButtons person =
  div []
      [ button [ class "btn"
               , onClick <| GoToPersonTree person.id
               ]
               [ text "Построить древо" ]
      , button [ class "btn"
               , onClick <| GoToPersonSiblings person.id
               ]
               [ text "Найти братьев и сестер" ]
      ]

dialogConfig : Person -> Dialog.Config Msg
dialogConfig person =
    { closeMessage = Just Close
    , containerClass = Nothing
    , header = Just <| drawPersonName person
    , body = Just <| drawPersonInfo person
    , footer = Just <| drawButtons person
    }

view : Model -> Html Msg
view model =
  if model.show
    then case model.person of
      Just person -> Dialog.view (Just <| dialogConfig person)
      Nothing -> div [] []
    else div [] []
