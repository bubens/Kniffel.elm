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
        counter : Int -> Array Int -> Array Int
        counter =
            \x ->
                \array ->
                    Array.update (x - 1) ((+) 1) array
    in
    getDicesetAsInts diceset
        |> List.foldl counter (Array.repeat 6 0)
        |> Array.toList


hasNEqualFaces : Int -> Diceset -> Bool
hasNEqualFaces n diceset =
    diceset
        |> countEachFace
        |> List.member n


isStraightOfLength : Int -> Diceset -> Bool
isStraightOfLength len diceset =
    let
        faceInList : Int -> String -> String
        faceInList =
            \x ->
                \acc ->
                    if x > 0 then
                        acc ++ "+"

                    else
                        acc ++ "-"
    in
    diceset
        |> countEachFace
        |> List.foldl faceInList ""
        |> String.contains (String.repeat len "+")


isAlwaysTrue : Rule
isAlwaysTrue diceset =
    True


isFullHouse : Rule
isFullHouse diceset =
    diceset
        |> hasNEqualFaces 3
        |> (&&) (diceset |> hasNEqualFaces 2)


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
isSmallStraight diceset =
    diceset
        |> isStraightOfLength 4


isLargeStraight : Rule
isLargeStraight diceset =
    diceset
        |> isStraightOfLength 5
