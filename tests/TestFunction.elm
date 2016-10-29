module TestFunction exposing (testFunction)

import Test exposing (..)
import Expect exposing (equal)

testFunction : String -> (Int -> Float -> String) -> List (Float, String, String, String) -> Test
testFunction name function data =
  describe name
    <| List.concat
    <| List.map
        (\(d, d0, d1, d2) -> 
          [ (0, d0), (1, d1), (2, d2) ]
          |> List.map
              (\(n, dn) ->
                test 
                  (name ++ " "++(toString n)++" "++(toString d)++" to "++dn) 
                  <| \() -> equal dn (function n d)
              )
        )
        data
