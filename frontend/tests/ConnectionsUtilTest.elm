module ConnectionsUtilTest  exposing (..)

import Test                 exposing (..)
import Expect

import TestUtils            exposing (..)

import ConnectionsUtil


all : Test
all =
  describe "ConnectionsUtil"
    [ test "extracts connections" <|
      \() -> ConnectionsUtil.calculateConnectingLines testPersonWithAll
      |> Expect.equal [(2,22),(1,2),(1,3),(6,65),(4,6),(4,5),(1,4)]
      ]
