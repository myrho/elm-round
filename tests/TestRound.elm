module TestRound exposing (roundTest)

import Test exposing (..)
import TestFunction exposing (..)
import Round


data =
    [ TestRow 0 "0" "0" "0" "0.0" "0.00"
    , TestRow 0 "0" "0" "0" "0.0" "0.00"
    , TestRow 0 "0" "0" "0" "0.0" "0.00"
    , TestRow 0 "0" "0" "0" "0.0" "0.00"
    , TestRow 0 "0" "0" "0" "0.0" "0.00"
    , TestRow 99 "100" "100" "99" "99.0" "99.00"
    , TestRow 9.9 "0" "10" "10" "9.9" "9.90"
    , TestRow 0.99 "0" "0" "1" "1.0" "0.99"
    , TestRow 0.099 "0" "0" "0" "0.1" "0.10"
    , TestRow 0.0099 "0" "0" "0" "0.0" "0.01"
    , TestRow -99 "-100" "-100" "-99" "-99.0" "-99.00"
    , TestRow -9.9 "0" "-10" "-10" "-9.9" "-9.90"
    , TestRow -0.99 "0" "0" "-1" "-1.0" "-0.99"
    , TestRow -0.099 "0" "0" "0" "-0.1" "-0.10"
    , TestRow -0.0099 "0" "0" "0" "0.0" "-0.01"
    , TestRow 1 "0" "0" "1" "1.0" "1.00"
    , TestRow 1.1 "0" "0" "1" "1.1" "1.10"
    , TestRow 1.01 "0" "0" "1" "1.0" "1.01"
    , TestRow 1.001 "0" "0" "1" "1.0" "1.00"
    , TestRow -1 "0" "0" "-1" "-1.0" "-1.00"
    , TestRow -1.1 "0" "0" "-1" "-1.1" "-1.10"
    , TestRow -1.01 "0" "0" "-1" "-1.0" "-1.01"
    , TestRow -1.001 "0" "0" "-1" "-1.0" "-1.00"
    , TestRow 213 "200" "210" "213" "213.0" "213.00"
    , TestRow 213.1 "200" "210" "213" "213.1" "213.10"
    , TestRow 213.01 "200" "210" "213" "213.0" "213.01"
    , TestRow 213.001 "200" "210" "213" "213.0" "213.00"
    , TestRow -213 "-200" "-210" "-213" "-213.0" "-213.00"
    , TestRow -213.1 "-200" "-210" "-213" "-213.1" "-213.10"
    , TestRow -213.01 "-200" "-210" "-213" "-213.0" "-213.01"
    , TestRow -213.001 "-200" "-210" "-213" "-213.0" "-213.00"
    , TestRow 5.5 "0" "10" "6" "5.5" "5.50"
    , TestRow 5.55 "0" "10" "6" "5.6" "5.55"
    , TestRow 5.555 "0" "10" "6" "5.6" "5.56"
    , TestRow 5.5555 "0" "10" "6" "5.6" "5.56"
    , TestRow -5.5 "0" "-10" "-5" "-5.5" "-5.50"
    , TestRow -5.55 "0" "-10" "-6" "-5.5" "-5.55"
    , TestRow -5.555 "0" "-10" "-6" "-5.6" "-5.55"
    , TestRow -5.5555 "0" "-10" "-6" "-5.6" "-5.56"
    , TestRow 5.5 "0" "10" "6" "5.5" "5.50"
    , TestRow 5.51 "0" "10" "6" "5.5" "5.51"
    , TestRow 5.501 "0" "10" "6" "5.5" "5.50"
    , TestRow 5.5001 "0" "10" "6" "5.5" "5.50"
    , TestRow -5.5 "0" "-10" "-5" "-5.5" "-5.50"
    , TestRow -5.51 "0" "-10" "-6" "-5.5" "-5.51"
    , TestRow -5.501 "0" "-10" "-6" "-5.5" "-5.50"
    , TestRow -5.5001 "0" "-10" "-6" "-5.5" "-5.50"
    , TestRow 4.9 "0" "0" "5" "4.9" "4.90"
    , TestRow 4.99 "0" "0" "5" "5.0" "4.99"
    , TestRow 4.999 "0" "0" "5" "5.0" "5.00"
    , TestRow 4.9999 "0" "0" "5" "5.0" "5.00"
    , TestRow -4.9 "0" "0" "-5" "-4.9" "-4.90"
    , TestRow -4.99 "0" "0" "-5" "-5.0" "-4.99"
    , TestRow -4.999 "0" "0" "-5" "-5.0" "-5.00"
    , TestRow -4.9999 "0" "0" "-5" "-5.0" "-5.00"
    , TestRow 1.234e22 "12340000000000000000000" "12340000000000000000000" "12340000000000000000000" "12340000000000000000000.0" "12340000000000000000000.00"
    , TestRow -1.234e22 "-12340000000000000000000" "-12340000000000000000000" "-12340000000000000000000" "-12340000000000000000000.0" "-12340000000000000000000.00"
    , TestRow 1.234e-22 "0" "0" "0" "0.0" "0.00"
    , TestRow -1.234e-22 "0" "0" "0" "0.0" "0.00"
    , TestRow (0 / 0) "NaN" "NaN" "NaN" "NaN" "NaN"
    , TestRow (1 / 0) "Infinity" "Infinity" "Infinity" "Infinity" "Infinity"
    ]


roundTest : Test
roundTest =
    testFunction "round" Round.round data
