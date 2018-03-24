port module Main exposing (..)

import AncestorsTest
import DescendantsTest
import RoutesTest
import MenuTest
import Test             exposing (concat)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode      exposing (Value)


main : TestProgram
main =
    run emit (Test.concat
        [ AncestorsTest.all
        , DescendantsTest.all
        , RoutesTest.all
        , MenuTest.all
        ])


port emit : ( String, Value ) -> Cmd msg
