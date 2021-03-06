port module Main exposing (..)

import Html             exposing (..)
import Html.Attributes  exposing (..)
import Html
import Navigation

import PersonTree
import PersonSiblings
import Login
import Menu
import Routes           exposing (..)
import Global           exposing (Msg(..))
import ServerApi        exposing (Jwt)

port save : String -> Cmd msg
port remove : () -> Cmd msg
port drawConnections : List (Int, Int) -> Cmd msg



type alias Flags = {
    jwt : String
  , initDate : String
  }

main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { personTreeModel : PersonTree.Model
    , personSiblingsModel : PersonSiblings.Model
    , jwt : Maybe Jwt
    , loginModel: Login.Model
    , menuModel : Menu.Model
    , route : Routes.Route
    , previousRoute : Maybe Routes.Route
    , initDate : String
    }


type Msg
    = PersonTreeMsg PersonTree.Msg
    | PersonSiblingsMsg PersonSiblings.Msg
    | GlobalMsg Global.Msg
    | LoginMsg Login.Msg
    | MenuMsg Menu.Msg
    | Navigate String
    | UrlChange Navigation.Location

initialModel : Flags -> Model
initialModel flags =
    { route = LoginPage
    , previousRoute = Nothing
    , personTreeModel = PersonTree.init flags.initDate
    , personSiblingsModel = PersonSiblings.init flags.initDate
    , menuModel = Menu.init
    , loginModel = Login.init
    , jwt = if flags.jwt /= "" then Just flags.jwt else Nothing
    , initDate = flags.initDate
    }



init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags loc =
    let ( subMdl, subCmd ) = update (UrlChange loc) <| initialModel flags
    in subMdl ! [ Cmd.map MenuMsg Menu.mountCmd, subCmd ]



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        PersonTreeMsg m ->
            let ( subMdl, subCmd ) =
                PersonTree.update m (Maybe.withDefault "" model.jwt) model.personTreeModel
            in { model | personTreeModel = subMdl } !
                [ Cmd.map PersonTreeMsg subCmd, drawConnections subMdl.connections ]

        PersonSiblingsMsg m ->
            let ( subMdl, subCmd ) =
                PersonSiblings.update m (Maybe.withDefault "" model.jwt) model.personSiblingsModel
            in { model | personSiblingsModel = subMdl } !
                [ Cmd.map PersonSiblingsMsg subCmd ]

        GlobalMsg m ->
            case m of
                None -> model ! []
                SaveJwt jwt -> { model | jwt = Just jwt } ! [ save jwt ]
                RemoveJwt -> { model | jwt = Nothing } ! [ remove () ]

        LoginMsg m ->
            let
                ( subMdl, subCmd, globalMsg ) =
                    Login.update m model.loginModel (model.previousRoute == Just LoginPage)
                ( mdl, cmd ) = update (GlobalMsg globalMsg) { model | loginModel = subMdl }
            in mdl ! [ Cmd.map LoginMsg subCmd, cmd ]

        MenuMsg m ->
            case model.jwt of
                Just jwt ->
                    let ( subMdl, subCmd ) = Menu.update m model.menuModel jwt
                    in { model | menuModel = subMdl } ! [ Cmd.map MenuMsg subCmd ]
                Nothing -> ( model, Cmd.none )

        Navigate url ->
            model ! [ Navigation.newUrl url ]

        UrlChange loc ->
            urlUpdate loc model




urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate loc model =
    let previousRoute = Just model.route in
    case (Routes.decode loc) of
        Nothing  ->
            model ! [ Navigation.modifyUrl (Routes.encode model.route) ]

        Just ((PersonTreePage personId) as route) ->
            case model.jwt of
                Just jwt ->
                    { model | previousRoute = previousRoute
                            , route = route
                            , personTreeModel = PersonTree.init model.initDate}
                        ! [ Cmd.map PersonTreeMsg <| PersonTree.mountCmd personId jwt ]
                Nothing -> ( model, Cmd.none )

        Just ((PersonSiblingsPage personId) as route) ->
            case model.jwt of
                Just jwt ->
                    { model | previousRoute = previousRoute
                            , route = route
                            , personSiblingsModel = PersonSiblings.init model.initDate}
                        ! [ Cmd.map PersonSiblingsMsg <| PersonSiblings.mountCmd personId jwt ]
                Nothing -> ( model, Cmd.none )

        Just (LoginPage as route) ->
            { model | previousRoute = previousRoute
                    , route = route }
                ! [ Cmd.map LoginMsg Login.mountCmd ]


view : Model -> Html Msg
view model =
    let showMenu = model.route /= LoginPage in
    div [ style [ ("display", "flex")
                , ("flex-direction", "column")
                , ("min-height", "100vh")
                , ("min-width", "100vw")
                , ("position", "absolute")
                , ("align-items", "center")
                ] ]
        [ if showMenu
          then Html.map MenuMsg <| Menu.view model.menuModel
          else div [] []
        , contentView model
        ]


contentView : Model -> Html Msg
contentView model =
    case model.route of

        PersonTreePage id ->
            Html.map PersonTreeMsg <| PersonTree.view model.personTreeModel

        PersonSiblingsPage id ->
            Html.map PersonSiblingsMsg <| PersonSiblings.view model.personSiblingsModel

        LoginPage ->
            Html.map LoginMsg <| Login.view model.loginModel
