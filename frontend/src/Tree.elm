module Tree                 exposing ( Model
                                     , Msg
                                     , init
                                     , view
                                     , update
                                     , mountCmd )

import Html                 exposing (..)
import Html.Attributes      exposing ( class
                                     , href
                                     , style
                                     , title
                                     )
import Html.Events          exposing ( onClick )
import Http

import ServerApi            exposing (..)
import Routes
import Global               exposing ( handleServerError
                                     , onlyUpdateModel )


type alias Model =
    { persons : List Person
    , error : Maybe String
    }


type Msg
    = HandleTreeRetrieved (Result Http.Error Person)


init : Model
init =
    Model [] Nothing


mountCmd : Cmd Msg
mountCmd =
    ServerApi.getAncestors 1 HandleTreeRetrieved


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of

        HandleTreeRetrieved res ->
            case res of
                Result.Ok person ->
                    onlyUpdateModel { model |
                        persons = [person],
                        error = Nothing }

                Result.Err err -> handleServerError model err



view : Model -> Html Msg
view model =
    div [] []


