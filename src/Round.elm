module Round
    exposing
        ( round
        , ceiling
        , floor
        , roundCom
        , ceilingCom
        , floorCom
        , roundNum
        , floorNum
        , ceilingNum
        , roundNumCom
        , floorNumCom
        , ceilingNumCom
        , toDecimal
        , truncate
        )

{-| This library converts a `Float` to a `String` with ultimate
control how many digits after the decimal point are shown and how the remaining
digits are rounded. It rounds, floors and ceils the "common" way (ie. [half
up](https://en.wikipedia.org/wiki/Rounding#Round_half_up)) or the "commerical"
way (ie. [half away from
zero](https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero)).

Example:

    x = 3.141592653589793

    round 2 x -- "3.14"
    round 4 x -- "3.1416"

    ceiling 2 x -- "3.15"
    floor 4 x -- "3.1415"

The given number of digits after decimal point can also be negative.

    x = 213.14

    round -2 x -- "200"
    round -1 x -- "210"

    ceiling -2 x -- "300"
    floor -3 x -- "0"

[Commercial
rounding](https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero)
means that negative and positive numbers are treated symmetrically. It affects
numbers whose last digit equals 5. For example:

    x = -0.5

    round 0 x -- "0"
    roundCom 0 x -- "-1"

    floor 0 x -- "-1"
    floorCom 0 x -- "0"

    ceiling 0 x -- "0"
    ceilingCom 0 x -- "-1"

Have a look at the tests for more examples!

Why couldn't you just do `x * 1000 |> round |> toFloat |> (flip (/)) 1000` in
order to round to 3 digits after comma? Due to floating point
arithmetic it might happen that it results into someting like
`3.1416000000001`,
although we just wanted `3.1416`.

Under the hood this library converts the `Float` into a `String` and rounds it
char-wise. Hence it's safe from floating point arithmetic weirdness.


# Round to String

@docs round, ceiling, floor, roundCom, ceilingCom, floorCom


# Round to Float

@docs roundNum, ceilingNum, floorNum, roundNumCom, ceilingNumCom, floorNumCom


# Utility functions

@docs toDecimal, truncate

-}

import String
import Char
import Tuple exposing (first, mapFirst)


{-| Like Elm's basic `truncate` but works on the full length of a float's 64
bits. So it's more precise.

    x = 9007199254740.99

    Basics.truncate x -- 652835028 (which is not correct)
    Round.truncate x -- 9007199254740 (which is)

-}
truncate : Float -> Int
truncate n =
    if n < 0 then
        Basics.ceiling n
    else
        Basics.floor n


{-| Transforms a `Float` in scientific notation into its decimal representation
as a `String`.

    x = 1e30
    toDecimal x -- outputs "1000000000000000000000000000000"

    x = 1.2345e-30
    toDecimal x -- outputs "0.0000000000000000000000000000012345"

-}
toDecimal : Float -> String
toDecimal fl =
    case abs fl |> Basics.toString |> String.split "e" of
        num :: exp :: _ ->
            let
                e =
                    (if String.startsWith "+" exp then
                        String.dropLeft 1 exp
                     else
                        exp
                    )
                        |> String.toInt
                        |> Result.toMaybe
                        |> Maybe.withDefault 0

                ( before, after ) =
                    splitComma num

                total =
                    before ++ after

                zeroed =
                    if e < 0 then
                        String.repeat (abs e) "0"
                            ++ total
                            |> String.uncons
                            |> Maybe.map (mapFirst String.fromChar)
                            |> Maybe.map (\( a, b ) -> a ++ "." ++ b)
                            |> Maybe.withDefault "0"
                    else
                        String.padRight (e + 1) '0' total
            in
                (if fl < 0 then
                    "-"
                 else
                    ""
                )
                    ++ zeroed

        num :: _ ->
            (if fl < 0 then
                "-"
             else
                ""
            )
                ++ num

        _ ->
            ""


splitComma : String -> ( String, String )
splitComma str =
    case String.split "." str of
        before :: after :: _ ->
            ( before, after )

        before :: _ ->
            ( before, "0" )

        _ ->
            ( "0", "0" )


roundFun : (Bool -> String -> Bool) -> Int -> Float -> String
roundFun functor s fl =
    if isInfinite fl || isNaN fl then
        Basics.toString fl
    else
        let
            ( before, after ) =
                abs fl
                    |> toDecimal
                    |> splitComma

            signed =
                fl < 0

            r =
                String.length before + s

            normalized =
                (String.repeat (negate r + 1) "0")
                    ++ String.padRight r '0' (before ++ after)

            totalLen =
                String.length normalized

            roundDigitIndex =
                max 1 r

            increase =
                normalized
                    |> String.slice roundDigitIndex totalLen
                    |> functor signed

            remains =
                String.slice 0 roundDigitIndex normalized

            num =
                if increase then
                    String.reverse remains
                        |> String.uncons
                        |> Maybe.map increaseNum
                        |> Maybe.withDefault "1"
                        |> String.reverse
                else
                    remains

            numLen =
                String.length num

            numZeroed =
                if num == "0" then
                    num
                else if s <= 0 then
                    String.repeat (abs s) "0"
                        |> (++) num
                else if s < String.length after then
                    (String.slice 0 (numLen - s) num)
                        ++ "."
                        ++ (String.slice (numLen - s) numLen num)
                else
                    String.padRight s '0' after
                        |> (++) (before ++ ".")
        in
            numZeroed
                |> addSign signed


addSign : Bool -> String -> String
addSign signed str =
    let
        isNotZero =
            String.toList str
                |> List.any (\c -> c /= '0' && c /= '.')
    in
        (if signed && isNotZero then
            "-"
         else
            ""
        )
            ++ str


increaseNum : ( Char, String ) -> String
increaseNum ( head, tail ) =
    if head == '9' then
        case String.uncons tail of
            Nothing ->
                "01"

            Just headtail ->
                increaseNum headtail
                    |> String.cons '0'
    else
        let
            c =
                Char.toCode head
        in
            if c >= 48 && c < 57 then
                String.cons (Char.fromCode <| c + 1) tail
            else
                "0"


{-| Turns a `Float` into a `String` and
[rounds](https://en.wikipedia.org/wiki/Rounding#Round_half_up) it to the given
number of digits
after decimal point. Behaves like `Basics.round`.

    x = 3.141592653589793

    round 2 x -- "3.14"
    round 4 x -- "3.1416"

The number of digits after decimal point can also be negative.

    x = 213.35

    round -2 x -- "200"
    round -1 x -- "210"

-}
round : Int -> Float -> String
round =
    roundFun
        (\signed str ->
            case String.uncons str of
                Nothing ->
                    False

                Just ( '5', "" ) ->
                    not signed

                Just ( '5', _ ) ->
                    True

                Just ( int, _ ) ->
                    Char.toCode int
                        |> (\int ->
                                (int > 53 && signed)
                                    || (int >= 53 && not signed)
                           )
        )


{-| Turns a `Float` into a `String` and rounds it up to the given number of
digits after decimal point.

    x = 3.141592653589793

    ceiling 2 x -- "3.15"
    ceiling 4 x -- "3.1416"

The number of digits after decimal point can also be negative.

    x = 213.35

    ceiling -2 x -- "300"
    ceiling -1 x -- "220"

-}
ceiling : Int -> Float -> String
ceiling =
    roundFun
        (\signed str ->
            case String.uncons str of
                Nothing ->
                    False

                Just ( '0', rest ) ->
                    String.toList rest
                        |> List.any ((/=) '0')
                        |> (&&) (not signed)

                _ ->
                    not signed
        )


{-| Turns a `Float` into a `String` and rounds it down to the given number of
digits after decimal point.

    x = 3.141592653589793

    floor 2 x -- "3.14"
    floor 4 x -- "3.1415"

The number of digits after decimal point can also be negative.

    x = 213.35

    floor -2 x -- "200"
    floor -1 x -- "210"

-}
floor : Int -> Float -> String
floor =
    roundFun
        (\signed str ->
            case String.uncons str of
                Nothing ->
                    False

                Just ( '0', rest ) ->
                    String.toList rest
                        |> List.any ((/=) '0')
                        |> (&&) (signed)

                _ ->
                    signed
        )


{-| Turns a `Float` into a `String` and rounds it to the given number of digits
after decimal point [the commercial
way](https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero).

    x = -0.5

    round 0 x -- "0"
    roundCom 0 x -- "-1"

The number of digits after decimal point can also be negative.

-}
roundCom : Int -> Float -> String
roundCom =
    roundFun
        (\_ int ->
            String.uncons int
                |> Maybe.map first
                |> Maybe.withDefault '0'
                |> Char.toCode
                |> (<=) 53
        )


{-| Turns a `Float` into a `String` and rounds it down to the given number of
digits after decimal point the commercial way.

    x = -0.5

    floor 0 x -- "-1"
    floorCom 0 x -- "0"

The number of digits after decimal point can also be negative.

-}
floorCom : Int -> Float -> String
floorCom s fl =
    if fl < 0 then
        ceiling s fl
    else
        floor s fl


{-| Turns a `Float` into a `String` and rounds it up to the given number of
digits after decimal point the commercial way.

    x = -0.5

    ceiling 0 x -- "0"
    ceilingCom 0 x -- "-1"

The number of digits after decimal point can also be negative.

-}
ceilingCom : Int -> Float -> String
ceilingCom s fl =
    if fl < 0 then
        floor s fl
    else
        ceiling s fl


{-| As `round` but turns the resulting `String` back to a `Float`.
-}
roundNum : Int -> Float -> Float
roundNum =
    funNum round


{-| As `floor` but turns the resulting `String` back to a `Float`.
-}
floorNum : Int -> Float -> Float
floorNum =
    funNum floor


{-| As `ceiling` but turns the resulting `String` back to a `Float`.
-}
ceilingNum : Int -> Float -> Float
ceilingNum =
    funNum ceiling


{-| As `roundCom` but turns the resulting `String` back to a `Float`.
-}
roundNumCom : Int -> Float -> Float
roundNumCom =
    funNum roundCom


{-| As `floorCom` but turns the resulting `String` back to a `Float`.
-}
floorNumCom : Int -> Float -> Float
floorNumCom =
    funNum floorCom


{-| As `ceilingCom` but turns the resulting `String` back to a `Float`.
-}
ceilingNumCom : Int -> Float -> Float
ceilingNumCom =
    funNum ceilingCom


funNum : (Int -> Float -> String) -> Int -> Float -> Float
funNum fun s fl =
    fun s fl
        |> String.toFloat
        |> Result.toMaybe
        |> Maybe.withDefault (0 / 0)
