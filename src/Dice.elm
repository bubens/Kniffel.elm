module Dice exposing (Dice, create, roll, hold, toSVG)

{-| This module is a small helper to create, handle and visualize a Dice.


# Definition

@docs Dice


# Create

@docs create


# Handle

@docs roll, hold


# Visualize

@docs toSVG

-}

import Html
import Svg
import Array
import Svg.Attributes as Attributes


{-| Definition of a Type Dice.

    Dice 1 False

-}
type alias Dice =
    { face : Int
    , held : Bool
    }


{-| Create a Dice with a predefined face.
A newly created Dice will not be held.

    myDice =
        Dice.create 1


    -- -> { face = 1, held = False } : Dice.Dice

-}
create : Int -> Dice
create initFace =
    Dice initFace False


{-| Roll a dice.
Will only if the dice is not currently held.

    myDice = Dice.create 1
    Dice.roll 3 myDice
    -- -> { face = 3, held = False } : Dice.Dice

-}
roll : Int -> Dice -> Dice
roll newFace dice =
    if dice.held == False then
        { dice | face = newFace }
    else
        dice


{-| Hold a dice.
A held dice will not roll.

    myDice = Dice.create 1
    Dice.hold True myDice
    -- -> { face = 3, held = True } : Dice.Dice

-}
hold : Bool -> Dice -> Dice
hold isHeld dice =
    { dice | held = isHeld }


toCircle : ( Int, Int ) -> Svg.Svg msg
toCircle ( pcx, pcy ) =
    Svg.circle
        [ Attributes.cx (toString pcx)
        , Attributes.cy (toString pcy)
        , Attributes.r "10"
        , Attributes.fill "#000000"
        ]
        []


createDots : List Int -> List (Svg.Svg msg)
createDots requiredDots =
    requiredDots
        |> List.map (\x -> getCoords x)
        |> List.map toCircle


getBgColor : Dice -> String
getBgColor dice =
    if dice.held == True then
        "#787878"
    else
        "#FFFFFF"


{-| Get a visual representation of the dice as SVG

    model : Dice
    model =
        Dice.create 1

    view : Model -> Html msg
    view model =
        Dice.toSVG model

-}
toSVG : Dice -> Html.Html msg
toSVG dice =
    let
        dots =
            Array.get (dice.face - 1) requiredDots

        backgroundColor =
            getBgColor dice
    in
        case dots of
            Just dots ->
                createDots dots
                    |> (::)
                        (Svg.rect
                            [ Attributes.width "100"
                            , Attributes.height "100"
                            , Attributes.x "0"
                            , Attributes.y "0"
                            , Attributes.fill (getBgColor dice)
                            ]
                            []
                        )
                    |> Svg.svg
                        [ Attributes.width "100"
                        , Attributes.height "100"
                        , Attributes.viewBox "0 0 100 100"
                        ]

            Nothing ->
                Svg.svg
                    [ Attributes.width "100"
                    , Attributes.height "100"
                    , Attributes.viewBox "0 0 100 100"
                    ]
                    [ Svg.text_
                        [ Attributes.x "25"
                        , Attributes.y "25"
                        , Attributes.fill "red"
                        ]
                        [ Svg.text "Error" ]
                    ]


requiredDots : Array.Array (List Int)
requiredDots =
    Array.fromList
        [ [ 3 ]
        , [ 5, 1 ]
        , [ 5, 3, 1 ]
        , [ 0, 1, 5, 6 ]
        , [ 0, 1, 3, 5, 6 ]
        , [ 0, 1, 2, 4, 5, 6 ]
        ]


getCoords : Int -> ( Int, Int )
getCoords position =
    let
        coords =
            Array.get position dotCoords
    in
        case coords of
            Just coords ->
                coords

            Nothing ->
                getCoords 0


dotCoords : Array.Array ( Int, Int )
dotCoords =
    Array.fromList
        [ ( 20, 20 )
        , ( 80, 20 )
        , ( 20, 50 )
        , ( 50, 50 )
        , ( 80, 50 )
        , ( 20, 80 )
        , ( 80, 80 )
        ]
