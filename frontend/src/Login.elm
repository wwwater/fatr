module Login exposing (Model, Msg, init, view, update, mountCmd)

import Html             exposing (..)
import Html.Attributes  exposing ( style
                                 , value
                                 , placeholder
                                 , maxlength
                                 , type_
                                 , autofocus
                                 )
import Html.Events      exposing ( onInput, onClick )
import Http
import Navigation       exposing ( back )

import ServerApi        exposing (..)
import Global           exposing ( Msg(..) )
import Styles           exposing (..)
import CommonHtml       exposing (..)
import Routes           exposing (..)


type alias Model =
    { credentials : Credentials
    , error : Maybe String
    }


type Msg
    = HandleJwtReceived (Result Http.Error Jwt)
    | ChangePassword String
    | SubmitCredentials
    | None


init : Model
init =
    Model { username = "user", password = "" } Nothing


mountCmd : Cmd Msg
mountCmd = Cmd.none


update : Msg -> Model -> Bool -> ( Model, Cmd Msg, Global.Msg )
update action model cameFromLogin =
    case action of
        HandleJwtReceived res ->
            case res of
                Result.Ok jwt ->
                    let _ = Debug.log "Received jwt" jwt in
                    ( { model | credentials = { username = "user", password = "" } }
                    , if cameFromLogin
                      then Routes.navigate (Routes.AncestorsPage 0)
                      else Navigation.back 1
                    , Global.SaveJwt jwt
                    )

                Result.Err err ->
                    let _ = Debug.log "Error getting jwt" err in
                    ( { model |
                          credentials = { username = "user", password = "" }
                        , error = Just "Наверное, это был неправильный ответ на вопрос(ᵔᴥᵔ)" }
                    , Cmd.none
                    , Global.RemoveJwt
                    )

        ChangePassword newPassword ->
            let creds = model.credentials in
            ( { model | credentials = { creds | password = newPassword } }
            , Cmd.none
            , Global.None
            )

        SubmitCredentials ->
            if model.credentials.password /= ""
                then
                    let _ = Debug.log "Submitting credentials for" model.credentials.username
                    in (
                        model
                        , ServerApi.getJwt model.credentials HandleJwtReceived
                        , Global.None
                        )
            else (
                model
                , Cmd.none
                , Global.None
                )
        None -> ( model, Cmd.none, Global.None )

drawError : Model -> Html Msg
drawError model =
    case model.error of
        Just error -> h2 [] [ text error ]
        Nothing -> div [] []



view : Model -> Html Msg
view model =
    div [ pageStyle
        , style [ ("flex-direction", "column")
                , ("align-items", "center") ]
        ]
        [ input [ style [ ("width", "400px")
                        , ("background-color", "#eee")
                        , ("color", "#333")
                        , ("text-align", "center")
                        , ("box-shadow", "inset 2px 2px 5px 0 #bbb")
                        , ("margin-bottom", "20px") ]
                , formStyle
                , type_ "password"
                , value model.credentials.password
                , placeholder "Остановка электрички, где у мамы дача"
                , maxlength 100
                , autofocus True
                , onKeyUp (\k ->
                    case k of
                        13 -> SubmitCredentials
                        _ -> None)
                , onInput ChangePassword ] []
        , button [ style [ ("width", "200px")
                         , ("background-color", "rgb(123,153,55)")
                         , ("box-shadow", "2px 2px 5px 1px #555")
                         , ("color", "#333")
                         , ("font-weight", "bold") ]
                 , formStyle
                 , onClick SubmitCredentials ]
            [ text "Войти" ]
        , drawError model
        ]

