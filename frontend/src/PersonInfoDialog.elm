module PersonInfoDialog exposing (Model, Msg, init, view, update, mountCmd)

import Html             exposing (..)
import Html.Attributes  exposing ( style
                                 , class
                                 , src
                                 , alt
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
    , date : String
    , error : Maybe String
    }


type Msg
    = HandlePersonRetrieved (Result Http.Error Person)
    | Close
    | GoToPersonTree Int
    | GoToPersonSiblings Int


init : String -> Model
init initDate = Model Nothing False initDate Nothing

mountCmd : Int -> Jwt -> Cmd Msg
mountCmd personId jwt =
  ServerApi.getPersonWithAbout personId jwt HandlePersonRetrieved


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

drawPhoto : Person -> Html Msg
drawPhoto person =
  case person.photo of
    Nothing -> div [] [ text "Фотографии нет" ]
    Just photo ->
      img [ src photo
          , alt "[Фотография]"
          , style [ ("max-width", "40%")
                  , ("max-height", "50vh")
                  ]
          ] []

drawPersonInfo : Person -> String -> Html Msg
drawPersonInfo person today =
  div [ style [ ("display", "flex")
              , ("flex-direction", "row")
              , ("align-items", "flex-start")
              , ("justify-content", "space-between")
              ] ]
      [ drawPhoto person
      , div [ style [ ("display", "flex")
                    , ("flex-direction", "column")
                    , ("padding", "0 10px")
                    , ("max-width", "60%")
                    , ("font-weight", "normal")
                    , ("overflow-y", "auto")
                    , ("max-height", "50vh")
                    ] ]
            [ div [ style [ ("margin-bottom", "10px") ] ]
                  [ formatDatesWithAge person today ]
            , div []
                  [ text <| Maybe.withDefault "" person.about ]
            ]
      ]

drawButtons : Person -> Html Msg
drawButtons person =
  div []
      [ button [ class "btn"
               , style [ ("margin", "5px") ]
               , onClick <| GoToPersonTree person.id
               ]
               [ text "Построить древо" ]
      , button [ class "btn"
               , style [ ("margin", "5px") ]
               , onClick <| GoToPersonSiblings person.id
               ]
               [ text "Найти братьев и сестер" ]
      ]

dialogConfig : Person -> String -> Dialog.Config Msg
dialogConfig person today =
    { closeMessage = Just Close
    , containerClass = Nothing
    , header = Just <| drawPersonName person
    , body = Just <| drawPersonInfo person today
    , footer = Just <| drawButtons person
    }

view : Model -> Html Msg
view model =
  if model.show
    then case model.person of
      Just person -> Dialog.view (Just <| dialogConfig person model.date)
      Nothing -> div [] []
    else div [] []
