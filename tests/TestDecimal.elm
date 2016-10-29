module TestDecimal exposing (decimalTest)

import Test exposing (..)
import Expect exposing (equal)

import TestData exposing (dataFloat)
import Round


scientific =
  [ 1.2345e-18
  , 1.2345e-17
  , 1.2345e-16
  , 1.2345e16
  , 1.2345e17
  , 1.2345e18
  , -1.2345e-18
  , -1.2345e-17
  , -1.2345e-16
  , -1.2345e16
  , -1.2345e17
  , -1.2345e18
  ]

decimal =
  [ "0.0000000000000000012345"
  , "0.000000000000000012345"
  , "0.00000000000000012345"
  , "12345000000000000"
  , "123450000000000000"
  , "1234500000000000000"
  , "-0.0000000000000000012345"
  , "-0.000000000000000012345"
  , "-0.00000000000000012345"
  , "-12345000000000000"
  , "-123450000000000000"
  , "-1234500000000000000"
  ]

decimalTest : Test
decimalTest =
    describe "scientific"
       ( List.map2
          (\a b ->
            test ("toDecimal "++(toString b)++" to "++a)
            <| \() -> equal a (Round.toDecimal b))
          decimal
          scientific
       )

