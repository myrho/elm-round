module TestCeil exposing (ceilTest)

import Test exposing (..)
import Expect exposing (equal)

import TestData exposing (dataFloat)
import Round

dataCeil0 =
  [ "0"
  , "0"
  , "0"
  , "0"
  , "0"
  , "99"
  , "10"
  , "1"
  , "1"
  , "1"
  , "-99"
  , "-9"
  , "0"
  , "0"
  , "0"
  , "1"
  , "2"
  , "2"
  , "2"
  , "-1"
  , "-1"
  , "-1"
  , "-1"
  , "213"
  , "214"
  , "214"
  , "214"
  , "-213"
  , "-213"
  , "-213"
  , "-213"
  , "6"
  , "6"
  , "6"
  , "6"
  , "-5"
  , "-5"
  , "-5"
  , "-5"
  , "6"
  , "6"
  , "6"
  , "6"
  , "-5"
  , "-5"
  , "-5"
  , "-5"
  , "5"
  , "5"
  , "5"
  , "5"
  , "-4"
  , "-4"
  , "-4"
  , "-4"
  ]

dataCeil1 =
  [ "0.0"
  , "0.0"
  , "0.0"
  , "0.0"
  , "0.0"
  , "99.0"
  , "9.9"
  , "1.0"
  , "0.1"
  , "0.1"
  , "-99.0"
  , "-9.9"
  , "-0.9"
  , "0.0"
  , "0.0"
  , "1.0"
  , "1.1"
  , "1.1"
  , "1.1"
  , "-1.0"
  , "-1.1"
  , "-1.0"
  , "-1.0"
  , "213.0"
  , "213.1"
  , "213.1"
  , "213.1"
  , "-213.0"
  , "-213.1"
  , "-213.0"
  , "-213.0"
  , "5.5"
  , "5.6"
  , "5.6"
  , "5.6"
  , "-5.5"
  , "-5.5"
  , "-5.5"
  , "-5.5"
  , "5.5"
  , "5.6"
  , "5.6"
  , "5.6"
  , "-5.5"
  , "-5.5"
  , "-5.5"
  , "-5.5"
  , "4.9"
  , "5.0"
  , "5.0"
  , "5.0"
  , "-4.9"
  , "-4.9"
  , "-4.9"
  , "-4.9"
  ]

dataCeil2 =
  [ "0.00"
  , "0.00"
  , "0.00"
  , "0.00"
  , "0.00"
  , "99.00"
  , "9.90"
  , "0.99"
  , "0.10"
  , "0.01"
  , "-99.00"
  , "-9.90"
  , "-0.99"
  , "-0.09"
  , "0.00"
  , "1.00"
  , "1.10"
  , "1.01"
  , "1.01"
  , "-1.00"
  , "-1.10"
  , "-1.01"
  , "-1.00"
  , "213.00"
  , "213.10"
  , "213.01"
  , "213.01"
  , "-213.00"
  , "-213.10"
  , "-213.01"
  , "-213.00"
  , "5.50"
  , "5.55"
  , "5.56"
  , "5.56"
  , "-5.50"
  , "-5.55"
  , "-5.55"
  , "-5.55"
  , "5.50"
  , "5.51"
  , "5.51"
  , "5.51"
  , "-5.50"
  , "-5.51"
  , "-5.50"
  , "-5.50"
  , "4.90"
  , "4.99"
  , "5.00"
  , "5.00"
  , "-4.90"
  , "-4.99"
  , "-4.99"
  , "-4.99"
  ]

ceilTest : Test
ceilTest =
    describe "ceil"
      <| ( List.map2
            (\a b -> 
              test ("ceil 0 "++(toString b)++" to "++a) 
              <| \() -> equal a (Round.ceiling 0 b))
            dataCeil0
            dataFloat
         )
      ++ ( List.map2
            (\a b -> 
              test ("ceil 1 "++(toString b)++" to "++a) 
              <| \() -> equal a (Round.ceiling 1 b))
            dataCeil1
            dataFloat
         )
      ++ ( List.map2
            (\a b -> 
              test ("ceil 2 "++(toString b)++" to "++a) 
              <| \() -> equal a (Round.ceiling 2 b))
            dataCeil2
            dataFloat
         )

