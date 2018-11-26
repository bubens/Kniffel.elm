module Dice exposing
    ( Dice
    , create
    , roll, hold
    , toSVG
    )

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

import Array
import Html
import Svg
import Svg.Attributes as Attributes


type Face
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


{-| Definition of a Type Dice.

    Dice 1 False

-}
type alias Dice =
    { face : Face
    , held : Bool
    }


{-| Create a Dice with a predefined face.
A newly created Dice will not be held.


    myDice =
        Dice.create 1

    -- -> { face = 1, held = False } : Dice.Dice

-}
create : Face -> Dice
create initFace =
    Dice initFace False


{-| Roll a dice.
Will only if the dice is not currently held.

    myDice = Dice.create 1
    Dice.roll 3 myDice
    -- -> { face = 3, held = False } : Dice.Dice

-}
roll : Face -> Dice -> Dice
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


createDots : Int -> List Int -> List (Svg.Svg msg)
createDots width dots =
    dots
        |> List.map (\x -> getCoords x)
        |> List.map
            (\( pcx, pcy ) ->
                let
                    widthFloat =
                        toFloat width

                    x =
                        String.fromInt (round (pcx * widthFloat))

                    --
                    y =
                        String.fromInt (round (pcy * widthFloat))

                    r =
                        String.fromInt (round (widthFloat / 10))
                in
                Svg.circle
                    [ Attributes.cx x
                    , Attributes.cy y
                    , Attributes.r r
                    , Attributes.fill "#000000"
                    ]
                    []
            )


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
toSVG : Int -> Dice -> Html.Html msg
toSVG width dice =
    let
        widthString =
            String.fromInt width

        viewBoxString =
            "0 0 " ++ widthString ++ " " ++ widthString

        dots =
            Array.get (dice.face - 1) requiredDots

        backgroundColor =
            getBgColor dice
    in
    case dots of
        Just goodDots ->
            createDots width goodDots
                |> (::)
                    (Svg.rect
                        [ Attributes.width widthString
                        , Attributes.height widthString
                        , Attributes.x "0"
                        , Attributes.y "0"
                        , Attributes.fill (getBgColor dice)
                        ]
                        []
                    )
                |> Svg.svg
                    [ Attributes.width widthString
                    , Attributes.height widthString
                    , Attributes.viewBox viewBoxString
                    ]

        Nothing ->
            Svg.svg
                [ Attributes.width widthString
                , Attributes.height widthString
                , Attributes.viewBox viewBoxString
                ]
                [ Svg.text_
                    [ Attributes.x (String.fromInt (width // 4))
                    , Attributes.y (String.fromInt (width // 4))
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


getCoords : Int -> ( Float, Float )
getCoords position =
    let
        coords =
            Array.get position dotCoords
    in
    case coords of
        Just goodCoords ->
            goodCoords

        Nothing ->
            getCoords 0


dotCoords : Array.Array ( Float, Float )
dotCoords =
    Array.fromList
        [ ( 0.2, 0.2 )
        , ( 0.8, 0.2 )
        , ( 0.2, 0.5 )
        , ( 0.5, 0.5 )
        , ( 0.8, 0.5 )
        , ( 0.2, 0.8 )
        , ( 0.8, 0.8 )
        ]
