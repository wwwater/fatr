module Menu             exposing ( Model
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
import Http

import Routes
import ServerApi        exposing (..)
import Global           exposing ( handleServerError
                                 , onlyUpdateModel
                                 )
import CommonHtml       exposing (..)

import InputSearch      exposing (..)


type alias Model = {
      persons : List Person
    , inputSearchModel : InputSearch.Model Msg
    , error : Maybe String
    }


type Msg
    = HandleListRetrieved (Result Http.Error (List Person))
    | GoToPersonTree Int
    | Search String
    | InputSearchMsg (InputSearch.Msg Msg)
    | NoOp


init : Model
init = Model
    []
    (InputSearch.init
        "Найти человека по имени и/или фамилии.."
        Search
        NoOp)
     Nothing


mountCmd : Cmd Msg
mountCmd = Cmd.none


update : Msg -> Model -> Jwt -> ( Model, Cmd Msg )
update action model jwt =
    case action of
        HandleListRetrieved res ->
            case res of
                Result.Ok persons ->
                    let inputSearchModel = model.inputSearchModel in
                    ( { model |
                        inputSearchModel = { inputSearchModel |
                                             options = personsToOptions persons } }
                    , Cmd.none )
                Result.Err err ->
                    let inputSearchModel = model.inputSearchModel
                        newModel = { model |
                                     inputSearchModel = { inputSearchModel |
                                                          options = [] } } in
                    handleServerError newModel err

        GoToPersonTree id ->
            ( model , Routes.navigate (Routes.PersonTreePage id) )

        Search s ->
            ( model, ServerApi.searchPersons s jwt HandleListRetrieved )

        InputSearchMsg msg ->
            let ( subMdl, subCmd, subMsg ) = InputSearch.update msg model.inputSearchModel
                ( mdl, cmd ) = update subMsg { model | inputSearchModel = subMdl } jwt
            in mdl ! [ Cmd.map InputSearchMsg subCmd, cmd ]

        NoOp -> ( model, Cmd.none )


personsToOptions : List Person -> List (InputSearch.Option Msg)
personsToOptions persons =
      List.map (\p ->
          { text = displayAsSearchEntry p, onClick = GoToPersonTree p.id } )
    <|List.sortBy (\p -> Maybe.withDefault "" p.birthday) persons


view : Model -> Html Msg
view model = div [ style [ ("background-color", "#999")
                         , ("display", "flex")
                         , ("justify-content", "center")
                         , ("border-radius", "10px")
                         , ("padding", "20px") ] ]
                 [ Html.map InputSearchMsg <| InputSearch.view model.inputSearchModel ]
