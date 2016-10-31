module Round exposing 
  ( roundNum, floorNum, ceilingNum
  , floorNumCom, ceilingNumCom
  , toDecimal
  , round, ceiling, floor, truncate
  , roundCom, ceilingCom, floorCom
  )

import String

truncate : Float -> Int
truncate n =
  if n < 0
    then Basics.ceiling n
    else Basics.floor n

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

ceiling : Int -> Float -> String
ceiling =
  roundFun Basics.ceiling

floor : Int -> Float -> String
floor =
  roundFun Basics.floor

{-| Away from zero
-}
roundCom : Int -> Float -> String
roundCom =
  roundFun 
    (\fl ->
      let
        dec = 
          fl - (toFloat <| truncate fl)
      in
        if dec >= 0.5
          then
            Basics.ceiling fl
        else if dec <= -0.5
          then 
            Basics.floor fl
          else
            Basics.round fl
    )

floorCom : Int -> Float -> String
floorCom s fl =
  if fl < 0
    then 
      ceiling s fl
    else
      floor s fl

ceilingCom : Int -> Float -> String
ceilingCom s fl =
  if fl < 0
    then 
      floor s fl
    else
      ceiling s fl

roundNum : Int -> Float -> Float
roundNum =
  funNum round

floorNum : Int -> Float -> Float
floorNum =
  funNum floor

ceilingNum : Int -> Float -> Float
ceilingNum =
  funNum ceiling

roundNumCom : Int -> Float -> Float
roundNumCom =
  funNum roundCom

floorNumCom : Int -> Float -> Float
floorNumCom =
  funNum floorCom

ceilingNumCom : Int -> Float -> Float
ceilingNumCom =
  funNum ceilingCom

funNum : (Int -> Float -> String) -> Int -> Float -> Float
funNum fun s fl =
  Maybe.withDefault (1/0)
  <| Result.toMaybe
  <| String.toFloat
  <| fun s fl

