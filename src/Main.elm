module Main exposing (Columns(..), Diceset, Edges, Entry, EntryName(..), Model, Msg(..), Sheet, Sums, decrementRollCounterBy, edges, errorEntry, getDiceWidth, getDicesetAsInts, getEarnedPoints, getSumOfValue, getValueWithKey, holdDiceAt, holdDiceset, init, initDiceset, initEntry, initSheet, initSums, main, pairWith, resetDiceset, returnIfRulePasses, rightOf, rollDiceMsg, rollDiceset, setRollCounter, setValueEntered, subscriptions, sumOfAll, sumOfFace, update, updateDiceset, updateEntry, updateGameOver, updateSums, valueToString, view, viewBonus, viewControlButton, viewControls, viewDiceset, viewEntry, viewGameOver, viewLeftSheet, viewPlaceholder, viewRightSheet, viewSheet, viewSum, viewSums)

import Array exposing (Array)
import Array.Extra as Array
import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Kniffel.Dice as Dice exposing (Dice)
import Kniffel.Rules as Rules
import List
import Random



-- MODEL


type alias Diceset =
    Array Dice


type EntryName
    = Empty
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | ThreeOfAKind
    | FourOfAKind
    | FullHouse
    | SmallStraight
    | LargeStraight
    | Yahtzee
    | Chance


type Columns
    = Left
    | Right
    | Error


type alias Entry =
    { name : EntryName
    , label : String
    , value : Maybe Int
    , entered : Bool
    , hovered : Bool
    , column : Columns
    }


type alias Sheet =
    Dict String Entry


type alias Sums =
    { left : Int
    , bonus : Maybe Int
    , right : Int
    , all : Int
    }


type alias Model =
    { diceset : Diceset
    , sheet : Sheet
    , rollsLeft : Int
    , valueEntered : Bool
    , sums : Sums
    , gameOver : Bool
    }


errorEntry : Entry
errorEntry =
    initEntry Empty "    " Nothing True True Error


initEntry : EntryName -> String -> Maybe Int -> Bool -> Bool -> Columns -> Entry
initEntry name label value entered hovered column =
    { name = name
    , label = label
    , value = value
    , entered = entered
    , hovered = hovered
    , column = column
    }


initDiceset : Diceset
initDiceset =
    Array.repeat 5 Dice.create


initSheet : Sheet
initSheet =
    Dict.fromList
        [ ( "error", errorEntry )
        , ( "ones", initEntry One "1er" Nothing False False Left )
        , ( "twos", initEntry Two "2er" Nothing False False Left )
        , ( "threes", initEntry Three "3er" Nothing False False Left )
        , ( "fours", initEntry Four "4er" Nothing False False Left )
        , ( "fives", initEntry Five "5er" Nothing False False Left )
        , ( "sixs", initEntry Six "6er" Nothing False False Left )
        , ( "threeOfAKind", initEntry ThreeOfAKind "3er-Pasch" Nothing False False Right )
        , ( "fourOfAKind", initEntry FourOfAKind "4er-Pasch" Nothing False False Right )
        , ( "fullHouse", initEntry FullHouse "FullHouse" Nothing False False Right )
        , ( "smallStraight", initEntry SmallStraight "Kleiner Straße" Nothing False False Right )
        , ( "largeStraight", initEntry LargeStraight "Große Straße" Nothing False False Right )
        , ( "yahtzee", initEntry Yahtzee "Yahztee" Nothing False False Right )
        , ( "chance", initEntry Chance "Chance" Nothing False False Right )
        ]


initSums : Sums
initSums =
    { left = 0
    , bonus = Nothing
    , right = 0
    , all = 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { diceset = initDiceset
      , sheet = initSheet
      , rollsLeft = 3
      , valueEntered = False
      , sums = initSums
      , gameOver = False
      }
    , rollDiceMsg
    )



-- UPDATE


type Msg
    = HoldDice Int
    | RollDice
    | DiceRolled (List Dice.Face)
    | EnterValue String
    | HoverValue String Bool
    | NextRound
    | Noop


rollDiceMsg : Cmd Msg
rollDiceMsg =
    Random.generate DiceRolled rollDiceset


getDicesetAsInts : Diceset -> List Int
getDicesetAsInts diceset =
    diceset
        |> Array.map (\dice -> Dice.toInt dice)
        |> Array.toList


getSumOfValue : Int -> List Int -> Int
getSumOfValue value faces =
    faces
        |> List.filter (\x -> x == value)
        |> List.foldr (+) 0


rollDiceset : Random.Generator (List Dice.Face)
rollDiceset =
    Dice.generateRandomFace
        |> Random.list 5


updateDiceset : List Dice.Face -> Model -> Model
updateDiceset result model =
    let
        newDiceset =
            Array.toList model.diceset
                |> List.map2
                    (\face dice ->
                        Dice.roll face dice
                    )
                    result
                |> Array.fromList
    in
    { model | diceset = newDiceset }


holdDiceAt : Int -> Model -> Model
holdDiceAt index model =
    let
        newDiceset =
            Array.indexedMap
                (\i dice ->
                    if i == index then
                        Dice.hold (not dice.held) dice

                    else
                        dice
                )
                model.diceset
    in
    { model | diceset = newDiceset }


sumOfFace : Int -> Diceset -> Int
sumOfFace x diceset =
    let
        sumIfFace : Int -> Int -> Int
        sumIfFace v a =
            if v == x then
                a + v

            else
                a
    in
    diceset
        |> getDicesetAsInts
        |> List.foldl sumIfFace 0


sumOfAll : Diceset -> Int
sumOfAll diceset =
    diceset
        |> getDicesetAsInts
        |> List.sum


returnIfRulePasses : Int -> Rules.Rule -> Diceset -> Int
returnIfRulePasses default rule diceset =
    if rule diceset then
        default

    else
        0


getEarnedPoints : Entry -> Diceset -> Int
getEarnedPoints entry diceset =
    case entry.name of
        Empty ->
            -404

        One ->
            sumOfFace 1 diceset

        Two ->
            sumOfFace 2 diceset

        Three ->
            sumOfFace 3 diceset

        Four ->
            sumOfFace 4 diceset

        Five ->
            sumOfFace 5 diceset

        Six ->
            sumOfFace 6 diceset

        ThreeOfAKind ->
            returnIfRulePasses
                (sumOfAll diceset)
                Rules.isThreeOfAKind
                diceset

        FourOfAKind ->
            returnIfRulePasses
                (sumOfAll diceset)
                Rules.isFourOfAKind
                diceset

        FullHouse ->
            returnIfRulePasses
                25
                Rules.isFullHouse
                diceset

        SmallStraight ->
            returnIfRulePasses
                30
                Rules.isSmallStraight
                diceset

        LargeStraight ->
            returnIfRulePasses
                40
                Rules.isLargeStraight
                diceset

        Yahtzee ->
            returnIfRulePasses
                50
                Rules.isYahtzee
                diceset

        Chance ->
            sumOfAll diceset


updateEntry : String -> Model -> Model
updateEntry key model =
    let
        newSheet =
            Dict.map
                (\k e ->
                    if k == key && e.entered == False then
                        { e
                            | value =
                                Just <|
                                    getEarnedPoints e model.diceset
                            , entered = True
                        }

                    else
                        e
                )
                model.sheet
    in
    { model | sheet = newSheet }


updateSums : Model -> Model
updateSums model =
    let
        sums =
            Dict.foldr
                (\key entry acc ->
                    let
                        v =
                            Maybe.withDefault 0 entry.value
                    in
                    case entry.column of
                        Left ->
                            { acc
                                | left = acc.left + v
                                , bonus =
                                    if (acc.left + v) >= 63 then
                                        Just 35

                                    else
                                        Nothing
                            }

                        Right ->
                            { acc
                                | right = acc.right + v
                            }

                        Error ->
                            acc
                )
                initSums
                model.sheet

        newSums =
            { sums
                | all =
                    Maybe.withDefault 0 sums.bonus + sums.left + sums.right
            }
    in
    { model | sums = newSums }


decrementRollCounterBy : Int -> Model -> Model
decrementRollCounterBy x model =
    { model | rollsLeft = model.rollsLeft - x }


setRollCounter : Int -> Model -> Model
setRollCounter counter model =
    { model | rollsLeft = counter }


setValueEntered : Bool -> Model -> Model
setValueEntered flag model =
    { model | valueEntered = flag }


holdDiceset : Model -> Model
holdDiceset model =
    let
        newDiceset =
            Array.map (Dice.hold True) model.diceset
    in
    { model | diceset = newDiceset }


resetDiceset : Model -> Model
resetDiceset model =
    let
        newDiceset =
            Array.map
                (\dice ->
                    dice
                        |> Dice.hold False
                        |> Dice.rollTo 1
                        |> Maybe.withDefault Dice.create
                )
                model.diceset
    in
    { model | diceset = newDiceset }


updateGameOver : Model -> Model
updateGameOver model =
    let
        newGameOver =
            Dict.foldr
                (\k value acc ->
                    acc && value.entered
                )
                True
                model.sheet
    in
    { model | gameOver = newGameOver }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HoldDice index ->
            model
                |> holdDiceAt index
                |> pairWith Cmd.none

        RollDice ->
            if model.rollsLeft > 0 && not model.valueEntered then
                ( model, rollDiceMsg )

            else
                ( model, Cmd.none )

        DiceRolled result ->
            model
                |> updateDiceset result
                |> decrementRollCounterBy 1
                |> pairWith Cmd.none

        EnterValue key ->
            if model.valueEntered == False then
                model
                    |> updateEntry key
                    |> updateSums
                    |> setValueEntered True
                    |> setRollCounter 0
                    |> holdDiceset
                    |> updateGameOver
                    |> pairWith Cmd.none

            else
                ( model, Cmd.none )

        HoverValue entryKey isOver ->
            if model.valueEntered == False then
                let
                    newSheet =
                        Dict.map
                            (\key entry ->
                                if key == entryKey then
                                    { entry | hovered = isOver }

                                else
                                    entry
                            )
                            model.sheet
                in
                { model | sheet = newSheet }
                    |> pairWith Cmd.none

            else
                ( model, Cmd.none )

        NextRound ->
            if model.valueEntered == True then
                model
                    |> setValueEntered False
                    |> setRollCounter 3
                    |> resetDiceset
                    |> pairWith rollDiceMsg

            else
                ( model, Cmd.none )

        Noop ->
            ( model, Cmd.none )



-- VIEW


getDiceWidth : Int -> Bool -> Int
getDiceWidth width isHeld =
    if isHeld == True then
        toFloat width
            |> (*) 0.8
            |> round

    else
        width


type alias Edges =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


edges : Edges
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


viewDiceset : Diceset -> Element Msg
viewDiceset diceset =
    diceset
        |> Array.indexedMap
            (\i dice ->
                Dice.toSvg (getDiceWidth 100 dice.held) dice
                    |> Element.html
                    |> el
                        [ centerX
                        , centerY
                        , height <| shrink
                        , width <| shrink
                        , Events.onMouseUp <| HoldDice i
                        ]
                    |> el
                        [ width <| fillPortion 1
                        , height fill
                        , centerX
                        , centerY
                        ]
            )
        |> Array.toList
        |> row
            [ width fill
            , height <| px 150
            ]


viewControlButton : Model -> Element Msg
viewControlButton model =
    let
        button =
            if model.gameOver then
                ( "GameOver", Nothing )

            else if model.valueEntered && model.rollsLeft == 0 then
                ( "Nächster Durchgang", Just NextRound )

            else
                ( "Würfeln", Just RollDice )
    in
    Input.button
        [ width <| px 280
        , height <| px 40
        , centerX
        , centerY
        , Background.color <| rgb255 170 170 170
        ]
        { onPress = Tuple.second button
        , label =
            el
                [ centerX
                , centerY
                , Font.variant Font.smallCaps
                , Font.size 18
                ]
                (text <| Tuple.first button)
        }


viewControls : Model -> Element Msg
viewControls model =
    row
        [ width fill
        , height <| px 50
        , centerX
        , centerY
        ]
        [ el
            [ height fill
            , width <| px 300
            ]
            (viewControlButton model)
        , el
            [ height shrink
            , width shrink
            , alignRight
            , centerY
            ]
            (text "Übrige Würfe: ")
        , el
            [ height shrink
            , width <| px 150
            , alignLeft
            , centerY
            , Font.size 30
            ]
            (text <| String.fromInt model.rollsLeft)
        ]


valueToString : Maybe Int -> String
valueToString v =
    case v of
        Just value ->
            if value > -1 then
                String.fromInt value
                    |> String.padLeft 3 ' '

            else
                valueToString Nothing

        Nothing ->
            "   "


viewEntry : String -> Sheet -> Element Msg
viewEntry k sheet =
    let
        createEl : ( String, Entry ) -> Element Msg
        createEl ( key, entry ) =
            el
                [ width fill
                , height <| px 50
                , centerY
                , centerX
                , Background.color
                    (if entry.hovered then
                        rgb255 178 178 178

                     else
                        rgb255 255 255 255
                    )
                , Events.onMouseUp
                    (if entry.entered then
                        Noop

                     else
                        EnterValue key
                    )
                , Events.onMouseEnter
                    (if entry.entered then
                        Noop

                     else
                        HoverValue key True
                    )
                , Events.onMouseLeave
                    (if entry.entered then
                        Noop

                     else
                        HoverValue key False
                    )
                ]
                (entry.value
                    |> valueToString
                    |> (++) (entry.label ++ ":")
                    |> text
                )
    in
    getValueWithKey k sheet
        |> Maybe.withDefault ( "error", errorEntry )
        |> createEl


viewLeftSheet : Sheet -> Element Msg
viewLeftSheet sheet =
    column
        [ width <| px 280
        , height fill
        ]
        [ viewEntry "ones" sheet
        , viewEntry "twos" sheet
        , viewEntry "threes" sheet
        , viewEntry "fours" sheet
        , viewEntry "fives" sheet
        , viewEntry "sixs" sheet
        , el
            [ width fill
            , height <| px 45
            ]
            none
        ]


viewRightSheet : Sheet -> Element Msg
viewRightSheet sheet =
    column
        [ width <| px 280
        , height fill
        ]
        [ viewEntry "threeOfAKind" sheet
        , viewEntry "fourOfAKind" sheet
        , viewEntry "fullHouse" sheet
        , viewEntry "smallStraight" sheet
        , viewEntry "largeStraight" sheet
        , viewEntry "yahtzee" sheet
        , viewEntry "chance" sheet
        ]


viewSheet : Sheet -> Element Msg
viewSheet sheet =
    row
        [ width fill
        , height <| px 390
        , centerX
        , padding 20
        ]
        [ viewLeftSheet sheet
        , viewRightSheet sheet
        ]


viewSum : String -> Int -> Element Msg
viewSum label value =
    let
        txt =
            String.fromInt value
                |> String.padLeft 4 ' '
                |> (++) (label ++ ":")
    in
    el
        [ width fill
        , height <| px 45
        ]
        (text txt)


viewBonus : String -> Maybe Int -> Element Msg
viewBonus label bonus =
    let
        txt =
            bonus
                |> Maybe.map (\v -> String.fromInt v)
                |> Maybe.withDefault " "
                |> String.padLeft 3 ' '
                |> (++) (label ++ ":")
    in
    el
        [ width fill
        , height <| px 45
        ]
        (text txt)


viewSums : Sums -> Element Msg
viewSums sums =
    column
        [ width fill
        , height <| px 155
        , paddingXY 20 0
        ]
        [ row
            [ width fill
            , height shrink
            ]
            [ column
                [ width <| px 270
                , height fill
                ]
                [ viewSum "Gesamt links" sums.left
                , viewBonus "Bonus links" sums.bonus
                ]
            , column
                [ width <| px 270
                , height fill
                ]
                [ viewSum "Gesamt rechts" sums.right
                ]
            ]
        , row
            [ width fill
            , height <| px 45
            , centerX
            ]
            [ viewSum "Gesamt" sums.all ]
        ]


viewPlaceholder : Element Msg
viewPlaceholder =
    el
        [ width fill
        , height <| px 50
        , Background.color <| rgb255 180 180 180
        ]
        (text "Here be content!")


viewGameOver : Model -> Attribute Msg
viewGameOver model =
    Element.inFront <|
        Element.el
            [ width <| px 500
            , height <| px 300
            , Border.width 5
            , Border.rounded 20
            , Border.color <| rgb255 0 0 0
            , Background.color <| rgb255 255 255 255
            , centerX
            , centerY
            ]
        <|
            column
                [ width fill
                , height fill
                ]
                [ el
                    [ width fill
                    , height <| fillPortion 2
                    , Font.size 40
                    , centerX
                    , centerY
                    ]
                    (text "Game Over")
                , el
                    [ width fill
                    , height <| fillPortion 3
                    , Font.size 30
                    , centerX
                    , centerY
                    ]
                    (text
                        ("Du hast "
                            ++ String.fromInt model.sums.all
                            ++ " Punkte erreicht."
                        )
                    )
                ]


view : Model -> Html Msg
view model =
    layout
        (if model.gameOver then
            [ viewGameOver model ]

         else
            []
        )
        (column
            [ width <| px 620
            , height <| px 820
            , Border.width 10
            , Border.color <| rgb255 0 0 0
            , Border.rounded 20
            ]
            [ viewDiceset model.diceset -- 150
            , viewControls model -- 50
            , viewSheet model.sheet --390
            , viewSums model.sums --> 180
            , viewPlaceholder -- 50
            ]
        )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- HELPER/UTILS


pairWith : b -> a -> ( a, b )
pairWith right left =
    ( left, right )


rightOf : a -> b -> ( a, b )
rightOf left right =
    ( left, right )


getValueWithKey : comparable -> Dict comparable v -> Maybe ( comparable, v )
getValueWithKey cmp dict =
    Dict.get cmp dict
        |> Maybe.map (\v -> ( cmp, v ))
