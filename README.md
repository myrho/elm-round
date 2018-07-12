# elm-round 

This library converts a `Float` to a `String` with ultimate 
control how many digits after the decimal point are shown and how the remaining 
digits are rounded. It rounds, floors and ceils the "common" way (ie. [half 
up](https://en.wikipedia.org/wiki/Rounding#Round_half_up)) or the "commerical" 
way (ie. [half away from 
zero](https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero)). 

Example:

```elm
x = 3.141592653589793

round 2 x -- "3.14"
round 4 x -- "3.1416"

ceiling 2 x -- "3.15"
floor 4 x -- "3.1415"
```

The given number of digits after decimal point can also be negative.

```elm
x = 213.14

round -2 x -- "200"
round -1 x -- "210"

ceiling -2 x -- "300"
floor -3 x -- "0"
```

[Commercial 
rounding](https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero) 
means that negative and positive numbers are treated symmetrically. It affects 
numbers whose last digit equals 5. For example:

```elm
x = -0.5

round 0 x -- "0"
roundCom 0 x -- "-1"

floor 0 x -- "-1"
floorCom 0 x -- "0"

ceiling 0 x -- "0"
ceilingCom 0 x -- "-1"
```

Have a look at the tests for more examples!

Why couldn't you just do `x * 1000 |> round |> toFloat |> (flip (/)) 1000` in 
order to round to 3 digits after comma? Due to floating point 
arithmetic it might happen that it results into someting like 
`3.1416000000001`, 
although we just wanted `3.1416`. 

Under the hood this library converts the `Float` into a `String` and rounds it 
char-wise. Hence it's safe from floating point arithmetic weirdness.

## Installation

From the root of your [Elm](http://elm-lang.org) project run

    elm package install myrho/elm-round

Import it in your Elm modules:

```elm
import Round 
```

## Releases

| Version | Notes |
| ------- | ----- |
| 1.0.3   | Fix issues with number in scientific notation, complete rewrite. |
| 1.0.2   | Given number of digits after decimal point can be negative. |
| 1.0.1   | Upgrade to Elm 0.18 |
| 1.0.0   | First official release, streamlined API and tests, docs added |
