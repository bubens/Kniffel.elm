module Main exposing (Diceset, Entry, EntryName(..), Model, Msg(..), Sheet, applyRuleAndGetPoints, getDiceWidth, getDicesetAsInts, getEarnedPoints, getSumOfValue, incrementRollCounter, init, initDiceset, initEntries, initEntry, leftOf, main, rightOf, rollDiceset, subscriptions, sumUpAll, sumUpFace, toggleValueEntered, update, updateDiceRolled, updateEnterValue, updateHoldDice, view)

import Array exposing (Array)
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
    = Dummy
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


type alias Entry =
    { name : EntryName
    , label : String
    , value : Maybe Int
    , entered : Bool
    }


type alias Sheet =
    Dict String Entry


type alias Model =
    { diceset : Diceset
    , sheet : Sheet
    , countRolls : Int
    , valueEntered : Bool
    }



-- INIT


initEntry : EntryName -> String -> Maybe Int -> Bool -> Entry
initEntry name value held =
    Entry name value held


initDiceset : Diceset
initDiceset =
    Array.repeat 5 Dice.create


initEntries : Sheet
initEntries =
    Dict.fromList
        [ ( "dummy", initEntry Dummy "Dummy" Nothing False )
        , ( "ones", initEntry One "1er" Nothing False )
        , ( "twos", initEntry Two "2er" Nothing False )
        , ( "threes", initEntry Three "3er" Nothing False )
        , ( "fours", initEntry Four "4er" Nothing False )
        , ( "fives", initEntry Five "5er" Nothing False )
        , ( "sixs", initEntry Six "6er" Nothing False )
        , ( "threeOfAKind", initEntry ThreeOfAKind "3er-Pasch" Nothing False )
        , ( "fourOfAKind", initEntry FourOfAKind "4er-Pasch" Nothing False )
        , ( "fullHouse", initEntry FullHouse "FullHouse" Nothing False )
        , ( "smallStraight", initEntry SmallStraight "Kleiner Straße" Nothing False )
        , ( "largeStraight", initEntry LargeStraight "Große Straße" Nothing False )
        , ( "yahtzee", initEntry Yahtzee "Yahztee" Nothing False )
        , ( "chance", initEntry Chance "Chance" Nothing False )
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        initDiceset
        initEntries
        0
        False
    , Cmd.none
    )



-- UPDATE


type Msg
    = HoldDice Int
    | RollDice
    | DiceRolled (List Dice.Face)
    | EnterValue String
    | NextRound


getDicesetAsInts : Diceset -> List Int
getDicesetAsInts diceset =
    diceset
        |> Array.map (\dice -> Dice.toInt dice)
        |> Array.toList


getSumOfValue : Int -> List Int -> Int
getSumOfValue value faces =
    faces
        |> List.filter (\x -> x == value)
        |> List.foldl (+) 0


rollDiceset : Random.Generator (List Dice.Face)
rollDiceset =
    Dice.generateRandomFace
        |> Random.list 5


updateHoldDice : Int -> Model -> Model
updateHoldDice index model =
    let
        mapper i dice =
            if i == index then
                Dice.hold (not dice.held) dice

            else
                dice

        newDiceset =
            Array.indexedMap mapper model.diceset
    in
    { model | diceset = newDiceset }


updateDiceRolled : List Dice.Face -> Model -> Model
updateDiceRolled result model =
    let
        newDiceset =
            Array.toList model.diceset
                |> List.map2 (\face -> \dice -> Dice.roll face dice) result
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
        Dummy ->
            -42

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
            applyRuleAndGetPoints (sumUpAll diceset) Rules.isThreeOfAKind diceset

        FourOfAKind ->
            applyRuleAndGetPoints (sumUpAll diceset) Rules.isFourOfAKind diceset

        FullHouse ->
            applyRuleAndGetPoints 25 Rules.isFullHouse diceset

        SmallStraight ->
            applyRuleAndGetPoints 30 Rules.isSmallStraight diceset

        LargeStraight ->
            applyRuleAndGetPoints 40 Rules.isLargeStraight diceset

        Yahtzee ->
            applyRuleAndGetPoints 50 Rules.isYahtzee diceset

        Chance ->
            sumUpAll diceset


updateEnterValue : String -> Model -> Model
updateEnterValue key model =
    { model
        | sheet =
            Dict.map
                (\k v ->
                    if k == key && v.entered == False then
                        { v
                            | value =
                                Just <|
                                    getEarnedPoints v model.diceset
                            , entered = True
                        }

                    else
                        v
                )
                model.sheet
    }


incrementRollCounter : Model -> Model
incrementRollCounter model =
    { model | countRolls = model.countRolls + 1 }


toggleValueEntered : Bool -> Model -> Model
toggleValueEntered flag model =
    { model | valueEntered = flag }


leftOf : b -> a -> ( a, b )
leftOf right left =
    ( left, right )


rightOf : a -> b -> ( a, b )
rightOf left right =
    ( left, right )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HoldDice index ->
            ( model, Cmd.none )
                |> Tuple.mapFirst (updateHoldDice index)

        RollDice ->
            if model.countRolls < 3 then
                model
                    |> incrementRollCounter
                    |> leftOf (Random.generate DiceRolled rollDiceset)

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
                    |> toggleValueEntered True
                    |> leftOf Cmd.none

            else
                ( model, Cmd.none )

        NextRound ->
            ( model, Cmd.none )



-- VIEW
{-
   onIngameClick : Msg -> Html.Attribute Msg
   onIngameClick message =
       Events.preventDefaultOn
           "click"
           (Json.map (\msg -> ( msg, True )) (Json.succeed message))
-}


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

            --, centerX
            --, centerY
            ]


viewButtonLabel : String -> Element Msg
viewButtonLabel str =
    text str
        |> el
            [ centerX
            , centerY
            , Font.variant Font.smallCaps
            , Font.size 20
            ]


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
            , width <| px 150
            , Background.color <| rgb 0 0 255
            ]
            (text "Weiter")
        , el
            [ height fill
            , width <| px 150
            ]
            (Input.button
                [ width <| px 100
                , height <| px 40
                , centerX
                , centerY
                , Background.color <| rgb255 170 170 170
                ]
                { onPress = Just RollDice
                , label = viewButtonLabel "Würfeln"
                }
            )
        , el
            [ height fill
            , width <| px 300
            , Background.color <| rgb255 255 0 0
            ]
            (text "Übrige Würfe: 1")
        ]


viewPlaceholder : Element Msg
viewPlaceholder =
    el
        [ width fill
        , height <| px 580
        , Background.color <| rgb255 180 180 180
        ]
        (text "Here be content!")


view : Model -> Html Msg
view model =
    layout
        []
    <|
        column
            [ width <| px 600
            , height <| px 800
            , Border.width 10
            , Border.color <| rgb255 0 0 0
            , Border.rounded 20

            --, explain Debug.todo
            ]
            [ viewDiceset model.diceset
            , viewControls model
            , viewPlaceholder
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
