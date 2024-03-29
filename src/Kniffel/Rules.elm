module Kniffel.Rules exposing
    ( Rule
    , isFourOfAKind
    , isFullHouse
    , isLargeStraight
    , isSmallStraight
    , isThreeOfAKind
    , isYahtzee
    )

import Array exposing (Array)
import Array.Extra as Array
import Kniffel.Dice as Dice


type alias Diceset =
    Array Dice.Dice


type alias Rule =
    Diceset -> Bool


asInts : Diceset -> List Int
asInts diceset =
    diceset
        |> Array.map Dice.toInt
        |> Array.toList


countEachFace : Diceset -> List Int
countEachFace diceset =
    let
        counter : Int -> Array Int -> Array Int
        counter x array =
            Array.update (x - 1) ((+) 1) array
    in
    diceset
        |> asInts
        |> List.foldl counter (Array.repeat 6 0)
        |> Array.toList


hasNEqualFaces : Int -> Diceset -> Bool
hasNEqualFaces n diceset =
    diceset
        |> countEachFace
        |> List.foldl
            (\x t -> (x >= n) || t)
            False


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


isFullHouse : Rule
isFullHouse diceset =
    diceset
        |> hasNEqualFaces 3
        |> (&&) (diceset |> hasNEqualFaces 2)


isThreeOfAKind : Rule
isThreeOfAKind diceset =
    diceset
        |> hasNEqualFaces 3


isFourOfAKind : Rule
isFourOfAKind diceset =
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
