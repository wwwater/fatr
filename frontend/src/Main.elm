port module Main exposing (..)

import Html             exposing (..)
import Html.Attributes  exposing (..)
import Html
import Navigation


import Ancestors
import Descendants
import Login
import Menu
import Routes           exposing (..)
import Global           exposing (Msg(..))
import ServerApi        exposing (Jwt)

port save : String -> Cmd msg
port remove : () -> Cmd msg


type alias Flags = { jwt : String }

main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { ancestorsModel : Ancestors.Model
    , descendantsModel : Descendants.Model
    , jwt : Maybe Jwt
    , loginModel: Login.Model
    , menuModel : Menu.Model
    , route : Routes.Route
    , previousRoute : Maybe Routes.Route
    }


type Msg
    = AncestorsMsg Ancestors.Msg
    | DescendantsMsg Descendants.Msg
    | GlobalMsg Global.Msg
    | LoginMsg Login.Msg
    | MenuMsg Menu.Msg
    | Navigate String
    | UrlChange Navigation.Location

initialModel : Flags -> Model
initialModel flags =
    { route = LoginPage
    , previousRoute = Nothing
    , ancestorsModel = Ancestors.init
    , descendantsModel = Descendants.init
    , menuModel = Menu.init
    , loginModel = Login.init
    , jwt = if flags.jwt /= "" then Just flags.jwt else Nothing
    }



init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags loc =
    let ( subMdl, subCmd ) = update (UrlChange loc) <| initialModel flags
    in subMdl ! [ Cmd.map MenuMsg Menu.mountCmd, subCmd ]



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        AncestorsMsg m ->
            let ( subMdl, subCmd ) = Ancestors.update m model.ancestorsModel
            in { model | ancestorsModel = subMdl } ! [ Cmd.map AncestorsMsg subCmd ]

        DescendantsMsg m ->
            let ( subMdl, subCmd ) = Descendants.update m model.descendantsModel
            in { model | descendantsModel = subMdl } ! [ Cmd.map DescendantsMsg subCmd ]

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

        Just ((AncestorsPage personId) as route) ->
            case model.jwt of
                Just jwt ->
                    { model | previousRoute = previousRoute
                            , route = route
                            , ancestorsModel = Ancestors.init }
                        ! [ Cmd.map AncestorsMsg <| Ancestors.mountCmd personId jwt ]
                Nothing -> ( model, Cmd.none )

        Just ((DescendantsPage personId) as route) ->
            case model.jwt of
                Just jwt ->
                    { model | previousRoute = previousRoute
                            , route = route
                            , descendantsModel = Descendants.init }
                        ! [ Cmd.map DescendantsMsg <| Descendants.mountCmd personId jwt ]
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
                         , ("float", "left")
                         ] ]
        [ if showMenu
          then Html.map MenuMsg <| Menu.view model.menuModel
          else div [] []
        , contentView model
        ]



contentView : Model -> Html Msg
contentView model =
    case model.route of

        AncestorsPage id ->
            Html.map AncestorsMsg <| Ancestors.view model.ancestorsModel

        DescendantsPage id ->
            Html.map DescendantsMsg <| Descendants.view model.descendantsModel

        LoginPage ->
            Html.map LoginMsg <| Login.view model.loginModel
