module Tests exposing (..)

import Test exposing (..)
import Expect exposing (equal)

import TestRound exposing (..)
import TestRoundMerc exposing (..)
import TestFloor exposing (..)
import TestFloorMerc exposing (..)
import TestCeil exposing (..)
import TestCeilMerc exposing (..)
import TestDecimal exposing (..)

all : Test
all =
  describe "All"
    [ roundTest
    , roundMercTest
    , ceilTest
    , ceilMercTest
    , floorTest
    , floorMercTest
    , decimalTest 
    ]

elmTest : Test
elmTest =
  describe "elmTest"
    [ test "test Basics.round" <| \() -> equal -5 (Basics.round -5.5)
    , test "test Basics.ceil" <| \() -> equal -1 (Basics.ceiling -1.1)
    , test "test Basics.ceil" <| \() -> equal -1 (Basics.ceiling -1.9)
    , test "test Basics.floor" <| \() -> equal -2 (Basics.floor -1.1)
    , test "test Basics.floor" <| \() -> equal -2 (Basics.floor -1.9)
    ]
