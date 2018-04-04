module Menu             exposing ( Model
                                 , Msg
                                 , init
                                 , view
                                 , update
                                 , mountCmd
                                 )

import Json.Decode      as Json

import Html             exposing (..)
import Html.Attributes  exposing ( style
                                 , class
                                 , title
                                 , id
                                 , autofocus
                                 , tabindex
                                 , placeholder
                                 , value
                                 )
import Html.Events      exposing ( onClick, on, keyCode, onInput )
import Http
import Dom
import Task

import Routes
import ServerApi        exposing (..)
import Global           exposing ( handleServerError
                                 , onlyUpdateModel
                                 )
import CommonHtml       exposing (..)


type alias Model = {
      persons : List Person
    , showSearchOptions : Bool
    , searchText : String
    , focusOnOption : Int
    , error : Maybe String
    }


type Msg
    = HandleListRetrieved (Result Http.Error (List Person))
    | ToggleSearchOptions Bool
    | FocusNextOption
    | FocusPreviousOption
    | None
    | FocusOn String
    | FocusResult (Result Dom.Error ())
    | GoToAncestors Int
    | Search String


init : Model
init = Model [] False "" 0 Nothing


mountCmd : Cmd Msg
mountCmd = Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        HandleListRetrieved res ->
            case res of
                Result.Ok persons ->
                    let newModel = { model | persons = persons } in
                        update (ToggleSearchOptions True) newModel
                Result.Err err ->
                    handleServerError { model | persons = [] } err

        ToggleSearchOptions show ->
             ( { model | showSearchOptions = show,
                         focusOnOption = 0,
                         searchText = if show then model.searchText else "" }
             , Cmd.none
             )

        FocusNextOption ->
            if (model.showSearchOptions)
            then
                let nextId = min (List.length model.persons) (model.focusOnOption + 1)
                    newModel = { model | focusOnOption = nextId }
                    msg = FocusOn  ("search-option-" ++ toString nextId)
                in update msg newModel
            else
                update (ToggleSearchOptions True) model
        FocusPreviousOption ->
            let previousId = max 0 (model.focusOnOption - 1)
                newModel = { model | focusOnOption = previousId }
                msg = FocusOn  ("search-option-" ++ toString previousId)
            in update msg newModel

        None -> onlyUpdateModel model

        FocusOn id ->
            ( model, Dom.focus id |> Task.attempt FocusResult )

        FocusResult result ->
            case result of
                Err (Dom.NotFound id) ->
                    let _ = Debug.log "unable to find dom" id in
                        ( model, Cmd.none )
                Ok () ->
                    ( model, Cmd.none )

        GoToAncestors id ->
            let ( mdl, cmd ) =
                update (ToggleSearchOptions False) { model | searchText = "" } in
                ( mdl, Routes.navigate (Routes.AncestorsPage id) )

        Search s ->
            if String.length s > 0
            then ( { model | searchText = s }, ServerApi.searchPersons s HandleListRetrieved )
            else update (ToggleSearchOptions False) { model | persons = [], searchText = s }



onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
  on "keyup" (Json.map tagger keyCode)


drawSearchOption : Int -> Person -> Int -> Html Msg
drawSearchOption index person focusOn =
    div [ style [ ("border-top", "1px solid #333")
                , ("padding", "10px")
                , ("background", if focusOn == index then "#ddd" else "transparent")
                , ("cursor", "pointer")
                ]
        , id ("search-option-" ++ toString index)
        , tabindex -1
        , onClick (GoToAncestors person.id)
        , onKeyUp (\k ->
            case k of
                40 -> FocusNextOption
                38 -> FocusPreviousOption
                13 -> GoToAncestors person.id
                _ -> None)
        ]
        [ text <| displayAsSearchEntry person ]


drawSearchOptions : List Person -> Int -> Html Msg
drawSearchOptions persons focusOn =
    div [ style [ ("display", "flex")
                , ("flex-direction", "column")
                , ("position", "absolute")
                , ("float", "right")
                , ("top", "56px")
                , ("width", "400px")
                , ("border-radius", "0 0 10px 10px")
                , ("background", "#eee")
                , ("z-index", "10")
                ] ]
        <| List.map (\(i, p) -> drawSearchOption i p focusOn)
        <| List.map2 (,) (List.range 1 (List.length persons))
        <| List.sortBy (\p -> Maybe.withDefault "" p.birthday) persons

drawSearchInput : String -> Html Msg
drawSearchInput searchText =
    input [ style [ ("display", "flex")
                  , ("width", "400px")
                  , ("padding", "10px")
                  , ("border", "none")
                  , ("border-radius", "10px")
                  , ("background", "#eee")
                  ]
          , autofocus True
          , id "search-option-0"
          , placeholder "Найти человека по имени и/или фамилии.."
          , onKeyUp (\k ->
              case k of
                  40 -> FocusNextOption
                  38 -> ToggleSearchOptions False
                  _ -> None)
          , onInput (\s -> Search s)
          , value searchText
          ]
          []

view : Model -> Html Msg
view model = div [ style [ ("background-color", "#777")
                         , ("display", "flex")
                         , ("justify-content", "center")
                         , ("padding", "20px") ] ]
                 [ drawSearchInput model.searchText
                 , if model.showSearchOptions
                   then drawSearchOptions model.persons model.focusOnOption
                   else div [] []
                 ]
