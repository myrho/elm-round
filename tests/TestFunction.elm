module TestFunction exposing (TestRow, testFunction)

import Test exposing (..)
import Expect exposing (equal)


type alias TestRow =
    { float : Float
    , round_2 : String
    , round_1 : String
    , round0 : String
    , round1 : String
    , round2 : String
    }


testFunction : String -> (Int -> Float -> String) -> List TestRow -> Test
testFunction name function data =
    List.indexedMap
        (\i { float, round_2, round_1, round0, round1, round2 } ->
            [ ( -2, round_2 ), ( -1, round_1 ), ( 0, round0 ), ( 1, round1 ), ( 2, round2 ) ]
                |> List.map
                    (\( n, dn ) ->
                        test
                            (String.fromInt i ++ ". " ++ name ++ " " ++ (String.fromInt n) ++ " " ++ (String.fromFloat float) ++ " to " ++ dn)
                        <|
                            \() -> equal dn (function n float)
                    )
        )
        data
        |> List.concat
        |> describe name
