port module Main exposing (..)

import ConnectionsUtilTest
import PersonTreeTest
import PersonSiblingsTest
import InputSearchTest
import LoginTest
import RoutesTest
import MenuTest
import Test             exposing (concat)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode      exposing (Value)


main : TestProgram
main =
    run emit (Test.concat
        [ PersonTreeTest.all
        , PersonSiblingsTest.all
        , ConnectionsUtilTest.all
        , InputSearchTest.all
        , LoginTest.all
        , RoutesTest.all
        , MenuTest.all
        ])


port emit : ( String, Value ) -> Cmd msg
