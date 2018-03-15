module Menu             exposing ( Model
                                 , Msg
                                 , init
                                 , view
                                 , update
                                 , mountCmd
                                 )

import Html             exposing (..)
import Html.Attributes  exposing ( style, class, title )
import Html.Events      exposing ( onClick )

import Routes
import ServerApi        exposing (..)


type alias Model = {}


type Msg
    = Ancestors Int

init : Model
init = Model


mountCmd : Cmd Msg
mountCmd = Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Ancestors personId ->
            (model, Routes.navigate <| Routes.AncestorsPage personId)


view : Html Msg
view = div [ style [ ("background-color", "#777")
                       , ("display", "flex")
                       , ("justify-content", "flex-end")
                       , ("padding", "16px 16px 0 16px") ] ]
           [ ]
