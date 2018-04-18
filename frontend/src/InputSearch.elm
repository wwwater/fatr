module InputSearch      exposing ( Model
                                 , Option
                                 , Msg
                                 , init
                                 , view
                                 , update
                                 , mountCmd
                                 )


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

type alias Option msg = {
      text : String
    , onClick : msg
    }

type alias Model msg = {
      options : List (Option msg)
    , placeholder : String
    , onSearch : String -> msg
    , searchText : String
    , focusOnOption : Int
    , noOp : msg
    }


type Msg msg
    = FocusNextOption
    | FocusPreviousOption
    | NoOp
    | FocusOn String
    | FocusResult (Result Dom.Error ())
    | OnOptionClick msg
    | OnSearch String (String -> msg)


init : String -> (String -> msg) -> msg -> Model msg
init placeholder onSearch noOp = Model [] placeholder onSearch "" 0 noOp


mountCmd : Cmd Msg
mountCmd = Cmd.none


update : Msg msg -> Model msg -> ( Model msg, Cmd (Msg msg), msg )
update action model =
    case action of

        FocusNextOption ->
            let nextId = min (List.length model.options) (model.focusOnOption + 1)
                newModel = { model | focusOnOption = nextId }
                msg = FocusOn  ("search-option-" ++ toString nextId)
            in update msg newModel

        FocusPreviousOption ->
            let previousId = max 0 (model.focusOnOption - 1)
                newModel = { model | focusOnOption = previousId }
                msg = FocusOn  ("search-option-" ++ toString previousId)
            in update msg newModel

        FocusOn id ->
            ( model, Dom.focus id |> Task.attempt FocusResult, model.noOp )

        FocusResult result ->
            case result of
                Err (Dom.NotFound id) ->
                    let _ = Debug.log "unable to find dom" id in
                        ( model, Cmd.none, model.noOp )
                Ok () ->
                    ( model, Cmd.none, model.noOp )

        OnOptionClick onOptionClick ->
            ( { model | searchText = "", options = [], focusOnOption = 0 }
            , Cmd.none, onOptionClick )

        OnSearch s action ->
            if String.length s > 0
            then ( { model | searchText = s }, Cmd.none, action s )
            else ( { model | searchText = s, options = [], focusOnOption = 0 }
                 , Cmd.none, model.noOp )

        NoOp -> ( model, Cmd.none, model.noOp )




drawOption : Int -> Option msg -> Int -> Html (Msg msg)
drawOption index option focusOn =
    div [ style [ ("border-top", "1px solid #333")
                , ("padding", "10px")
                , ("background", if focusOn == index then "#ddd" else "transparent")
                , ("cursor", "pointer")
                ]
        , id ("search-option-" ++ toString index)
        , tabindex -1
        , onClick (OnOptionClick option.onClick)
        , onKeyUp (\k ->
            case k of
                40 -> FocusNextOption
                38 -> FocusPreviousOption
                13 -> OnOptionClick option.onClick
                _ -> NoOp)
        ]
        [ text option.text ]


drawOptions : Model msg -> Html (Msg msg)
drawOptions model =
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
        <| List.map (\(i, option) -> drawOption i option model.focusOnOption)
        <| List.map2 (,) (List.range 1 (List.length model.options)) model.options

drawInput : Model msg -> Html (Msg msg)
drawInput model =
    input [ style [ ("display", "flex")
                  , ("width", "400px")
                  , ("padding", "10px")
                  , ("border", "none")
                  , ("border-radius", "10px")
                  , ("background", "#eee")
                  ]
          , autofocus True
          , id "search-option-0"
          , placeholder model.placeholder
          , onKeyUp (\k ->
              case k of
                  40 -> FocusNextOption -- down
                  _ -> NoOp)
          , onInput (\s -> OnSearch s model.onSearch)
          , value model.searchText
          ]
          []

view : Model msg -> Html (Msg msg)
view model = div [ style [
                         ("display", "flex")
                         ] ]
                 [ drawInput model
                 , drawOptions model
                 ]
