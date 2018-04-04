port module Main exposing (..)

import Html             exposing (..)
import Html.Attributes  exposing (..)
import Html
import Navigation

import Tree
import Ancestors
import Descendants
import Menu
import Routes           exposing (..)

port save : String -> Cmd msg
port remove : () -> Cmd msg



main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { route : Routes.Route
    , treeModel : Tree.Model
    , ancestorsModel : Ancestors.Model
    , descendantsModel : Descendants.Model
    , menuModel : Menu.Model
    }


type Msg
    = TreeMsg Tree.Msg
    | AncestorsMsg Ancestors.Msg
    | DescendantsMsg Descendants.Msg
    | MenuMsg Menu.Msg
    | Navigate String
    | UrlChange Navigation.Location


initialModel : Model
initialModel =
    { route = TreePage
    , treeModel = Tree.init
    , ancestorsModel = Ancestors.init
    , descendantsModel = Descendants.init
    , menuModel = Menu.init
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init loc =
    let ( subMdl, subCmd ) = update (UrlChange loc) <| initialModel
    in subMdl ! [ Cmd.map MenuMsg Menu.mountCmd, subCmd ]



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        TreeMsg m ->
            let ( subMdl, subCmd ) = Tree.update m model.treeModel
            in { model | treeModel = subMdl } ! [ Cmd.map TreeMsg subCmd ]

        AncestorsMsg m ->
            let ( subMdl, subCmd ) = Ancestors.update m model.ancestorsModel
            in { model | ancestorsModel = subMdl } ! [ Cmd.map AncestorsMsg subCmd ]

        DescendantsMsg m ->
            let ( subMdl, subCmd ) = Descendants.update m model.descendantsModel
            in { model | descendantsModel = subMdl } ! [ Cmd.map DescendantsMsg subCmd ]

        MenuMsg m ->
            let ( subMdl, subCmd ) = Menu.update m model.menuModel
            in { model | menuModel = subMdl } ! [ Cmd.map MenuMsg subCmd ]

        UrlChange loc ->
            urlUpdate loc model

        Navigate url ->
            model ! [ Navigation.newUrl url ]



urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate loc model =
    case (Routes.decode loc) of
        Nothing  ->
            model ! [ Navigation.modifyUrl (Routes.encode model.route) ]

        Just (TreePage as route) ->
            { model | route = route, treeModel = Tree.init }
                ! [ Cmd.map TreeMsg <| Tree.mountCmd ]

        Just ((AncestorsPage personId) as route) ->
            { model | route = route, ancestorsModel = Ancestors.init }
                ! [ Cmd.map AncestorsMsg <| Ancestors.mountCmd personId ]

        Just ((DescendantsPage personId) as route) ->
            { model | route = route, descendantsModel = Descendants.init }
                ! [ Cmd.map DescendantsMsg <| Descendants.mountCmd personId ]


view : Model -> Html Msg
view model = div [ style [ ("display", "flex")
                         , ("flex-direction", "column")
                         , ("min-height", "100vh")
                         , ("min-width", "100vw")
                         , ("float", "left")
                         ] ]
    [ Html.map MenuMsg <| Menu.view model.menuModel
    , contentView model
    ]



contentView : Model -> Html Msg
contentView model =
    case model.route of
        TreePage ->
            Html.map TreeMsg <| Tree.view model.treeModel

        AncestorsPage id ->
            Html.map AncestorsMsg <| Ancestors.view model.ancestorsModel

        DescendantsPage id ->
            Html.map DescendantsMsg <| Descendants.view model.descendantsModel
