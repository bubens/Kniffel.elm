module Main exposing (Diceset, Entry, EntryName(..), Model, Msg(..), Sheet, applyRuleAndGetPoints, getDiceWidth, getDicesetAsInts, getEarnedPoints, getSumOfValue, incrementRollCounter, init, initDiceset, initEntry, initSheet, leftOf, main, rightOf, rollDiceset, subscriptions, sumUpAll, sumUpFace, toggleValueEntered, update, updateDiceRolled, updateEnterValue, updateHoldDice, view)

import Array exposing (Array)
import Array.Extra as Array
import Browser
import Debug
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as Json
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
    , countRolls : Int
    , valueEntered : Bool
    , sums : Sums
    }


errorEntry : Entry
errorEntry =
    initEntry Empty "    " Nothing False Error


initEntry : EntryName -> String -> Maybe Int -> Bool -> Columns -> Entry
initEntry name label value entered column =
    { name = name
    , label = label
    , value = value
    , entered = entered
    , column = column
    }


initDiceset : Diceset
initDiceset =
    Array.repeat 5 Dice.create


initSheet : Sheet
initSheet =
    Dict.fromList
        [ ( "error", errorEntry )
        , ( "ones", initEntry One "1er" Nothing False Left )
        , ( "twos", initEntry Two "2er" Nothing False Left )
        , ( "threes", initEntry Three "3er" Nothing False Left )
        , ( "fours", initEntry Four "4er" Nothing False Left )
        , ( "fives", initEntry Five "5er" Nothing False Left )
        , ( "sixs", initEntry Six "6er" Nothing False Left )
        , ( "threeOfAKind", initEntry ThreeOfAKind "3er-Pasch" Nothing False Right )
        , ( "fourOfAKind", initEntry FourOfAKind "4er-Pasch" Nothing False Right )
        , ( "fullHouse", initEntry FullHouse "FullHouse" Nothing False Right )
        , ( "smallStraight", initEntry SmallStraight "Kleiner Straße" Nothing False Right )
        , ( "largeStraight", initEntry LargeStraight "Große Straße" Nothing False Right )
        , ( "yahtzee", initEntry Yahtzee "Yahztee" Nothing False Right )
        , ( "chance", initEntry Chance "Chance" Nothing False Right )
        ]


initRollCount : Int
initRollCount =
    1


initValueEntered : Bool
initValueEntered =
    False


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
      , countRolls = initRollCount
      , valueEntered = initValueEntered
      , sums = initSums
      }
    , rollDiceMsg
    )



-- UPDATE


type Msg
    = HoldDice Int
    | RollDice
    | DiceRolled (List Dice.Face)
    | EnterValue String
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


updateHoldDice : Int -> Model -> Model
updateHoldDice index model =
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


updateDiceRolled : List Dice.Face -> Model -> Model
updateDiceRolled result model =
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


sumUpFace : Int -> Diceset -> Int
sumUpFace x diceset =
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


sumUpAll : Diceset -> Int
sumUpAll diceset =
    diceset
        |> getDicesetAsInts
        |> List.sum


applyRuleAndGetPoints : Int -> Rules.Rule -> Diceset -> Int
applyRuleAndGetPoints default rule diceset =
    if rule diceset then
        default

    else
        0


getEarnedPoints : Entry -> Diceset -> Int
getEarnedPoints entry diceset =
    case entry.name of
        Empty ->
            -1

        One ->
            sumUpFace 1 diceset

        Two ->
            sumUpFace 2 diceset

        Three ->
            sumUpFace 3 diceset

        Four ->
            sumUpFace 4 diceset

        Five ->
            sumUpFace 5 diceset

        Six ->
            sumUpFace 6 diceset

        ThreeOfAKind ->
            applyRuleAndGetPoints
                (sumUpAll diceset)
                Rules.isThreeOfAKind
                diceset

        FourOfAKind ->
            applyRuleAndGetPoints
                (sumUpAll diceset)
                Rules.isFourOfAKind
                diceset

        FullHouse ->
            applyRuleAndGetPoints
                25
                Rules.isFullHouse
                diceset

        SmallStraight ->
            applyRuleAndGetPoints
                30
                Rules.isSmallStraight
                diceset

        LargeStraight ->
            applyRuleAndGetPoints
                40
                Rules.isLargeStraight
                diceset

        Yahtzee ->
            applyRuleAndGetPoints
                50
                Rules.isYahtzee
                diceset

        Chance ->
            sumUpAll diceset


updateEnterValue : String -> Model -> Model
updateEnterValue key model =
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


incrementRollCounter : Model -> Model
incrementRollCounter model =
    { model | countRolls = model.countRolls + 1 }


setRollCounter : Int -> Model -> Model
setRollCounter counter model =
    { model | countRolls = counter }


toggleValueEntered : Bool -> Model -> Model
toggleValueEntered flag model =
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HoldDice index ->
            ( model, Cmd.none )
                |> Tuple.mapFirst (updateHoldDice index)

        RollDice ->
            if model.countRolls < 3 && not model.valueEntered then
                model
                    |> incrementRollCounter
                    |> leftOf rollDiceMsg

            else
                ( model, Cmd.none )

        DiceRolled result ->
            model
                |> updateDiceRolled result
                |> leftOf Cmd.none

        EnterValue key ->
            if model.valueEntered == False then
                model
                    |> updateEnterValue key
                    |> updateSums
                    |> toggleValueEntered True
                    |> holdDiceset
                    |> setRollCounter 3
                    |> leftOf Cmd.none

            else
                ( model, Cmd.none )

        NextRound ->
            if model.valueEntered == True then
                model
                    |> toggleValueEntered False
                    |> setRollCounter 1
                    |> resetDiceset
                    |> leftOf rollDiceMsg

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
                        , height <| px 100
                        , Events.onMouseUp <| HoldDice i
                        ]
                    |> el
                        [ width <| fillPortion 1
                        , height fill
                        ]
            )
        |> Array.toList
        |> row
            [ width fill
            , height <| px 150
            ]


viewButton : String -> Msg -> Element Msg
viewButton lbl msg =
    Input.button
        [ width <| px 100
        , height <| px 40
        , centerX
        , centerY
        , Background.color <| rgb255 170 170 170
        ]
        { onPress = Just msg
        , label =
            el
                [ centerX
                , centerY
                , Font.variant Font.smallCaps
                , Font.size 18
                ]
                (text lbl)
        }


viewControls : Model -> Element Msg
viewControls model =
    row
        [ width fill
        , height <| px 50
        , centerX
        , centerY

        ----, -- explain Debug.todo
        ]
        [ el
            [ height fill
            , width <| px 150
            ]
            (viewButton "Weiter" NextRound)
        , el
            [ height fill
            , width <| px 150
            ]
            (viewButton "Würfeln" RollDice)
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
            (text <| String.fromInt (3 - model.countRolls))
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
                , Events.onMouseUp
                    (if entry.entered then
                        Noop

                     else
                        EnterValue key
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

        ----, -- explain Debug.todo
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

        --, -- explain Debug.todo
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

        --, -- explain Debug.todo
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


view : Model -> Html Msg
view model =
    layout
        []
    <|
        column
            [ width <| px 620
            , height <| px 820
            , Border.width 10
            , Border.color <| rgb255 0 0 0
            , Border.rounded 20

            --, -- explain Debug.todo
            ]
            [ viewDiceset model.diceset -- 150
            , viewControls model -- 50
            , viewSheet model.sheet --390
            , viewSums model.sums --> 180
            , viewPlaceholder -- 50
            ]



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


leftOf : b -> a -> ( a, b )
leftOf right left =
    ( left, right )


rightOf : a -> b -> ( a, b )
rightOf left right =
    ( left, right )


getValueWithKey : comparable -> Dict comparable v -> Maybe ( comparable, v )
getValueWithKey cmp dict =
    Dict.get cmp dict
        |> Maybe.map (\v -> ( cmp, v ))
