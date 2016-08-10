module Round exposing 
  ( roundNum, floorNum, ceilNum
  , floorNumMerc, ceilNumMerc
  , toDecimal, round, ceil, floor, truncate
  )

import String

roundNum : Int -> Float -> Float
roundNum =
  funNum round

floorNum : Int -> Float -> Float
floorNum =
  funNum floor

ceilNum : Int -> Float -> Float
ceilNum =
  funNum ceil

floorNumMerc : Int -> Float -> Float
floorNumMerc s fl =
  if fl < 0
    then 
      ceilNum s fl
    else
      floorNum s fl

ceilNumMerc : Int -> Float -> Float
ceilNumMerc s fl =
  if fl < 0
    then 
      floorNum s fl
    else
      ceilNum s fl

funNum : (Int -> Float -> String) -> Int -> Float -> Float
funNum fun s fl =
  Maybe.withDefault (1/0)
  <| Result.toMaybe
  <| String.toFloat
  <| fun s fl

toDecimal : Float -> String
toDecimal fl =
  case String.split "e"
        <| Basics.toString fl of
    num :: exp :: _ ->
      let
        e = 
          String.toInt exp
            |> Result.toMaybe
            |> Maybe.withDefault 0
        (sign, before,after) =
          let
            (b,a) =
              splitComma num
            hasSign =
              fl < 0
          in
            ( if hasSign
                then 
                  "-"
                else 
                  ""
            , if hasSign
                then 
                  String.dropLeft 1 b
                else
                  b
            , a
            )

        newBefore = 
          if e >= 0
            then 
              before
            else
              if abs e < String.length before
                then
                  String.left (String.length before - abs e) before
                  ++
                  "."
                  ++
                  String.right (abs e) before
                else
                  "0."
                  ++
                  String.repeat (abs e - String.length before) "0"
                  ++
                  before

        newAfter =
          if e <= 0
            then 
              after
            else
              if e < String.length after
                then
                  String.left e after
                  ++
                  "."
                  ++
                  String.right (String.length after - e) after
                else
                  after
                  ++
                  String.repeat (e - String.length after) "0"
      in
        sign ++ newBefore ++ newAfter
    num :: _ ->
      num
    _ ->
      ""


        
splitComma : String -> (String,String)
splitComma str =
  case String.split "." str of
    before :: after :: _ ->
      (before,after)
    before :: _ ->
      (before, "0")
    _ ->
      ("0","0")


roundFun : (Float -> Int) -> Int -> Float -> String
roundFun functor s fl =
  if s <= 0
    then 
      functor fl |> Basics.toString
    else
      let
        (before, after) =
          toDecimal fl
            |> splitComma 
        a = 
          after
            |> String.padRight (s+1) '0'

        b = String.left s a
        c = String.dropLeft s a 
        e = 10^s
        f =
          ( if fl < 0
            then "-"
            else ""
          ) ++"1"++b++"."++c
            |> String.toFloat
            |> Result.toMaybe
            |> Maybe.withDefault e
            |> functor
        n =
          if fl < 0
            then -1
            else 1
        dd =
          if fl < 0
            then 2
            else 1
        g =
          Basics.toString f |> String.dropLeft dd

        h =
          truncate fl
          + ( if f - (e*n) == (e*n)
                then 
                  if fl < 0
                    then -1
                    else 1
                else
                  0
            )

        j = Basics.toString h

        i =
          if j == "0" && f-(e*n) /= 0 && fl < 0 && fl > -1
            then "-" ++ j
            else j
      in
        i
          ++ "."
          ++ g

round : Int -> Float -> String
round =
  roundFun Basics.round

ceil : Int -> Float -> String
ceil =
  roundFun Basics.ceiling

floor : Int -> Float -> String
floor =
  roundFun Basics.floor

truncate : Float -> Int
truncate n =
  if n < 0
    then Basics.ceiling n
    else Basics.floor n
