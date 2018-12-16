module Rules exposing
    ( Rule
    , isAlwaysTrue
    , isFullHouse
    , isLargeStraight
    , isQuadruple
    , isSmallStraight
    , isTriple
    , isYahtzee
    )

import Array exposing (Array)
import Array.Extra as Array
import Dice


type alias Diceset =
    Array Dice.Dice


type alias Rule =
    Diceset -> Bool


getDicesetAsInts : Diceset -> List Int
getDicesetAsInts diceset =
    diceset
        |> Array.map (\dice -> Dice.toInt dice)
        |> Array.toList


countEachFace : Diceset -> List Int
countEachFace diceset =
    let
        counter =
            \x ->
                \array ->
                    Array.update x ((+) 1) array
    in
    getDicesetAsInts diceset
        |> List.foldl counter (Array.repeat 6 0)
        |> Array.toList


hasNEqualFaces : Int -> Diceset -> Bool
hasNEqualFaces n diceset =
    countEachFace diceset
        |> List.member n


isAlwaysTrue : Rule
isAlwaysTrue diceset =
    True


isFullHouse : Rule
isFullHouse diceset =
    let
        faces =
            countEachFace diceset
    in
    List.member 3 faces
        && List.member 2 faces


isTriple : Rule
isTriple diceset =
    diceset
        |> hasNEqualFaces 3


isQuadruple : Rule
isQuadruple diceset =
    diceset
        |> hasNEqualFaces 4


isYahtzee : Rule
isYahtzee diceset =
    diceset
        |> hasNEqualFaces 5


isSmallStraight : Rule
isSmallStraight =
    isAlwaysTrue


isLargeStraight : Rule
isLargeStraight =
    isAlwaysTrue
