module Tests where

import ElmTest exposing (..)

import Round

dataFloat =
  [ 0
  , 0.0
  , 0.00
  , 0.000
  , 0.0000
  , 99
  , 9.9
  , 0.99
  , 0.099
  , 0.0099
  , -99
  , -9.9
  , -0.99
  , -0.099
  , -0.0099
  , 1
  , 1.1
  , 1.01
  , 1.001
  , -1
  , -1.1
  , -1.01
  , -1.001
  , 213
  , 213.1
  , 213.01
  , 213.001
  , -213
  , -213.1
  , -213.01
  , -213.001
  , 5.5
  , 5.55
  , 5.555
  , 5.5555
  , -5.5
  , -5.55
  , -5.555
  , -5.5555
  , 4.9
  , 4.99
  , 4.999
  , 4.9999
  , -4.9
  , -4.99
  , -4.999
  , -4.9999
  ]

dataRound0 =
  [ "0"
  , "0"
  , "0"
  , "0"
  , "0"
  , "99"
  , "10"
  , "1"
  , "0"
  , "0"
  , "-99"
  , "-10"
  , "-1"
  , "0"
  , "0"
  , "1"
  , "1"
  , "1"
  , "1"
  , "-1"
  , "-1"
  , "-1"
  , "-1"
  , "213"
  , "213"
  , "213"
  , "213"
  , "-213"
  , "-213"
  , "-213"
  , "-213"
  , "6"
  , "6"
  , "6"
  , "6"
  , "-5" -- that's a feature, see https://msdn.microsoft.com/en-us/library/5cza0web(v=vs.94).aspx
  , "-6"
  , "-6"
  , "-6"
  , "5"
  , "5"
  , "5"
  , "5"
  , "-5"
  , "-5"
  , "-5"
  , "-5"
  ]

dataRound1 =
  [ "0.0"
  , "0.0"
  , "0.0"
  , "0.0"
  , "0.0"
  , "99.0"
  , "9.9"
  , "1.0"
  , "0.1"
  , "0.0"
  , "-99.0"
  , "-9.9"
  , "-1.0"
  , "-0.1"
  , "0.0"
  , "1.0"
  , "1.1"
  , "1.0"
  , "1.0"
  , "-1.0"
  , "-1.1"
  , "-1.0"
  , "-1.0"
  , "213.0"
  , "213.1"
  , "213.0"
  , "213.0"
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
  , "-5.6"
  , "-5.6"
  , "4.9"
  , "5.0"
  , "5.0"
  , "5.0"
  , "-4.9"
  , "-5.0"
  , "-5.0"
  , "-5.0"
  ]

dataRound2 =
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
  , "-0.10"
  , "-0.01"
  , "1.00"
  , "1.10"
  , "1.01"
  , "1.00"
  , "-1.00"
  , "-1.10"
  , "-1.01"
  , "-1.00"
  , "213.00"
  , "213.10"
  , "213.01"
  , "213.00"
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
  , "-5.56"
  , "4.90"
  , "4.99"
  , "5.00"
  , "5.00"
  , "-4.90"
  , "-4.99"
  , "-5.00"
  , "-5.00"
  ]

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
  , "4.90"
  , "4.99"
  , "5.00"
  , "5.00"
  , "-4.90"
  , "-4.99"
  , "-4.99"
  , "-4.99"
  ]

dataFloor0 =
  [ "0"
  , "0"
  , "0"
  , "0"
  , "0"
  , "99"
  , "9"
  , "0"
  , "0"
  , "0"
  , "-99"
  , "-10"
  , "-1"
  , "-1"
  , "-1"
  , "1"
  , "1"
  , "1"
  , "1"
  , "-1"
  , "-2"
  , "-2"
  , "-2"
  , "213"
  , "213"
  , "213"
  , "213"
  , "-213"
  , "-214"
  , "-214"
  , "-214"
  , "5"
  , "5"
  , "5"
  , "5"
  , "-6"
  , "-6"
  , "-6"
  , "-6"
  , "4"
  , "4"
  , "4"
  , "4"
  , "-5"
  , "-5"
  , "-5"
  , "-5"
  ]

dataFloor1 =
  [ "0.0"
  , "0.0"
  , "0.0"
  , "0.0"
  , "0.0"
  , "99.0"
  , "9.9"
  , "0.9"
  , "0.0"
  , "0.0"
  , "-99.0"
  , "-9.9"
  , "-1.0"
  , "-0.1"
  , "-0.1"
  , "1.0"
  , "1.1"
  , "1.0"
  , "1.0"
  , "-1.0"
  , "-1.1"
  , "-1.1"
  , "-1.1"
  , "213.0"
  , "213.1"
  , "213.0"
  , "213.0"
  , "-213.0"
  , "-213.1"
  , "-213.1"
  , "-213.1"
  , "5.5"
  , "5.5"
  , "5.5"
  , "5.5"
  , "-5.5"
  , "-5.6"
  , "-5.6"
  , "-5.6"
  , "4.9"
  , "4.9"
  , "4.9"
  , "4.9"
  , "-4.9"
  , "-5.0"
  , "-5.0"
  , "-5.0"
  ]

dataFloor2 =
  [ "0.00"
  , "0.00"
  , "0.00"
  , "0.00"
  , "0.00"
  , "99.00"
  , "9.90"
  , "0.99"
  , "0.09"
  , "0.00"
  , "-99.00"
  , "-9.90"
  , "-0.99"
  , "-0.10"
  , "-0.01"
  , "1.00"
  , "1.10"
  , "1.01"
  , "1.00"
  , "-1.00"
  , "-1.10"
  , "-1.01"
  , "-1.01"
  , "213.00"
  , "213.10"
  , "213.01"
  , "213.00"
  , "-213.00"
  , "-213.10"
  , "-213.01"
  , "-213.01"
  , "5.50"
  , "5.55"
  , "5.55"
  , "5.55"
  , "-5.50"
  , "-5.55"
  , "-5.56"
  , "-5.56"
  , "4.90"
  , "4.99"
  , "4.99"
  , "4.99"
  , "-4.90"
  , "-4.99"
  , "-5.00"
  , "-5.00"
  ]


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

all : Test
all =
  suite "All"
    [ roundTest, ceilTest, scientificTest 
    ]

roundTest : Test
roundTest =
    suite "round"
      <| ( List.map2
            (\a b -> 
              test ("round 0 "++(toString b)++" to "++a) 
              <| assertEqual a (Round.round 0 b))
            dataRound0
            dataFloat
         )
         ++ 
         ( List.map2
            (\a b -> 
              test ("round 1 "++(toString b)++" to "++a) 
              <| assertEqual a (Round.round 1 b))
            dataRound1
            dataFloat
         )
         ++ 
         ( List.map2
            (\a b -> 
              test ("round 2 "++(toString b)++" to "++a) 
              <| assertEqual a (Round.round 2 b))
            dataRound2
            dataFloat
         )

ceilTest : Test
ceilTest =
    suite "ceil"
      <| ( List.map2
            (\a b -> 
              test ("ceil 0 "++(toString b)++" to "++a) 
              <| assertEqual a (Round.ceil 0 b))
            dataCeil0
            dataFloat
         )
      ++ ( List.map2
            (\a b -> 
              test ("ceil 1 "++(toString b)++" to "++a) 
              <| assertEqual a (Round.ceil 1 b))
            dataCeil1
            dataFloat
         )
      ++ ( List.map2
            (\a b -> 
              test ("ceil 2 "++(toString b)++" to "++a) 
              <| assertEqual a (Round.ceil 2 b))
            dataCeil2
            dataFloat
         )

floorTest : Test
floorTest =
    suite "floor"
      <| ( List.map2
            (\a b -> 
              test ("floor 0 "++(toString b)++" to "++a) 
              <| assertEqual a (Round.floor 0 b))
            dataFloor0
            dataFloat
         )
      ++ ( List.map2
            (\a b -> 
              test ("floor 1 "++(toString b)++" to "++a) 
              <| assertEqual a (Round.floor 1 b))
            dataFloor1
            dataFloat
         )
      ++ ( List.map2
            (\a b -> 
              test ("floor 2 "++(toString b)++" to "++a) 
              <| assertEqual a (Round.floor 2 b))
            dataFloor2
            dataFloat
         )

scientificTest : Test
scientificTest =
    suite "scientific"
       ( List.map2
          (\a b ->
            test ("toDecimal "++(toString b)++" to "++a)
            <| assertEqual a (Round.toDecimal b))
          decimal
          scientific
       )

elmTest : Test
elmTest =
  suite "elmTest"
    [ test "test Basics.round" <| assertEqual -5 (Basics.round -5.5)
    , test "test Basics.ceil" <| assertEqual -1 (Basics.ceiling -1.1)
    , test "test Basics.ceil" <| assertEqual -1 (Basics.ceiling -1.9)
    , test "test Basics.floor" <| assertEqual -2 (Basics.floor -1.1)
    , test "test Basics.floor" <| assertEqual -2 (Basics.floor -1.9)
    ]
