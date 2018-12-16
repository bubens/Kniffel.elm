module Main exposing (Diceset, Model, Msg(..), createDiceListHtml, createListItemHtml, init, main, rollDiceset, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Debug
import Dice exposing (Dice)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Json
import List
import Random
import Rules



-- MODEL


type alias Diceset =
    Array Dice


type SumType
    = SumAll
    | SumValue Int
    | Predefined Int


type alias Entry =
    { name : String
    , sumType : SumType
    , value : Int
    , entered : Bool
    , rule : Rules.Rule
    }


type alias Controls =
    { countRolls : Int
    , valueEntered : Bool
    }


type alias Model =
    { diceset : Diceset
    , entries : List Entry
    , controls : Controls
    }



-- INIT


initEntry : String -> SumType -> Int -> Bool -> Rules.Rule -> Entry
initEntry name type_ value held =
    Entry name type_ value held


initDiceset : Diceset
initDiceset =
    Array.repeat 5 Dice.create


initEntries : List Entry
initEntries =
    [ initEntry "Ones" (SumValue 1) -1 False Rules.isAlwaysTrue
    , initEntry "Twos" (SumValue 2) -1 False Rules.isAlwaysTrue
    , initEntry "Threes" (SumValue 3) -1 False Rules.isAlwaysTrue
    , initEntry "Fours" (SumValue 4) -1 False Rules.isAlwaysTrue
    , initEntry "Fives" (SumValue 5) -1 False Rules.isAlwaysTrue
    , initEntry "Six" (SumValue 6) -1 False Rules.isAlwaysTrue
    , initEntry "All 3" SumAll -1 False Rules.isTriple
    , initEntry "All 4" SumAll -1 False Rules.isQuadruple
    , initEntry "Full House" (Predefined 35) -1 False Rules.isFullHouse
    , initEntry "Small Straight" (Predefined 30) -1 False Rules.isSmallStraight
    , initEntry "Large Straight" (Predefined 40) -1 False Rules.isLargeStraight
    , initEntry "Yahtzee" (Predefined 50) -1 False Rules.isYahtzee
    , initEntry "Chance" SumAll -1 False Rules.isAlwaysTrue
    ]


initControls : Controls
initControls =
    { countRolls = 0
    , valueEntered = False
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        initDiceset
        initEntries
        initControls
    , Cmd.none
    )



-- UPDATE


type Msg
    = HoldDice Int
    | RollDice
    | DiceRolled (List Dice.Face)
    | EnterValue Entry
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


getMaxPoints : Entry -> Model -> Int
getMaxPoints entry model =
    let
        faces =
            getDicesetAsInts model.diceset
    in
    case entry.sumType of
        SumValue face ->
            getSumOfValue face faces

        SumAll ->
            List.foldl (+) 0 faces

        Predefined value ->
            value


rollDiceset : Random.Generator (List Dice.Face)
rollDiceset =
    Random.list 5 <| Dice.generateRandomFace


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


updateEnterValue : Entry -> Model -> Model
updateEnterValue entry model =
    let
        name =
            entry.name

        maxPoints =
            getMaxPoints entry model

        updateEntries thisEntry =
            if thisEntry.name == name then
                if thisEntry.rule model.diceset == True then
                    { thisEntry | value = maxPoints, entered = True }

                else
                    { thisEntry | value = 0, entered = True }

            else
                thisEntry

        newEntries =
            List.map updateEntries model.entries
    in
    if not entry.entered then
        { model | entries = newEntries }

    else
        model


incrementRollCounter : Model -> Model
incrementRollCounter model =
    let
        curControls =
            model.controls

        curCounter =
            curControls.countRolls

        newControls =
            { curControls | countRolls = curCounter + 1 }
    in
    { model | controls = newControls }


toggleValueEntered : Bool -> Model -> Model
toggleValueEntered flag model =
    let
        curControls =
            model.controls

        newControls =
            { curControls | valueEntered = flag }
    in
    { model | controls = newControls }


toTupleWithCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
toTupleWithCmd msg model =
    ( model, msg )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HoldDice index ->
            ( model, Cmd.none )
                |> Tuple.mapFirst (updateHoldDice index)

        RollDice ->
            if model.controls.countRolls < 3 then
                model
                    |> incrementRollCounter
                    |> toTupleWithCmd (Random.generate DiceRolled rollDiceset)

            else
                ( model, Cmd.none )

        DiceRolled result ->
            model
                |> updateDiceRolled result
                |> toTupleWithCmd Cmd.none

        EnterValue entry ->
            if model.controls.valueEntered == False then
                model
                    |> updateEnterValue entry
                    |> toggleValueEntered True
                    |> toTupleWithCmd Cmd.none

            else
                ( model, Cmd.none )

        NextRound ->
            ( model, Cmd.none )



-- VIEW


onIngameClick : Msg -> Html.Attribute Msg
onIngameClick message =
    Events.preventDefaultOn
        "click"
        (Json.map (\msg -> ( msg, True )) (Json.succeed message))


getDiceWidth : Int -> Bool -> Int
getDiceWidth width isHeld =
    if isHeld == True then
        toFloat width
            |> (*) 0.8
            |> round

    else
        width


createListItemHtml : Int -> Dice -> Html Msg
createListItemHtml index dice =
    let
        width =
            getDiceWidth 100 dice.held
    in
    Html.li
        [ onIngameClick (HoldDice index)
        , Attributes.style "float" "left"
        ]
        [ Dice.toSvg width dice
        ]


createDiceListHtml : Diceset -> Html Msg
createDiceListHtml diceset =
    diceset
        |> Array.indexedMap createListItemHtml
        |> Array.toList
        |> Html.ul
            [ Attributes.style "list-style" "none"
            , Attributes.style "clear" "both"
            ]


getEntryValue : Int -> String
getEntryValue value =
    if value < 0 then
        " "

    else
        String.fromInt value


createEntriesRowHtml : Entry -> Html Msg
createEntriesRowHtml entry =
    Html.tr
        [ onIngameClick (EnterValue entry)
        ]
        [ Html.td [] [ Html.text <| entry.name ++ ":" ]
        , Html.td [] [ Html.text <| getEntryValue entry.value ]
        ]


createEntriesHtml : List Entry -> Html Msg
createEntriesHtml entries =
    entries
        |> List.map createEntriesRowHtml
        |> Html.table []


createRollCounterHtml : Controls -> Html Msg
createRollCounterHtml controls =
    Html.div
        [ Attributes.style "clear" "both"
        ]
        [ Html.text <| "Rolls: " ++ String.fromInt controls.countRolls ]


createButtonRollHtml : Controls -> Html Msg
createButtonRollHtml controls =
    let
        isDisabled =
            controls.countRolls == 3 || controls.valueEntered
    in
    Html.button
        [ onIngameClick RollDice
        , Attributes.style "clear" "both"
        , Attributes.style "display" "block"
        , Attributes.disabled isDisabled
        ]
        [ Html.text "Roll!"
        ]


createButtonNextRoundHtml : Controls -> Html Msg
createButtonNextRoundHtml controls =
    let
        isDisabled =
            not controls.valueEntered
    in
    Html.button
        [ onIngameClick NextRound
        , Attributes.disabled isDisabled
        ]
        [ Html.text "Next!"
        ]


createSumsHtml : List Entry -> Html Msg
createSumsHtml entries =
    let
        names =
            [ "Ones", "Twos", "Threes", "Fours", "Fives", "Six" ]

        folder entry reducer =
            if List.member entry.name names then
                if entry.entered then
                    { reducer | upper = reducer.upper + entry.value }

                else
                    reducer

            else if entry.entered then
                { reducer | lower = reducer.lower + entry.value }

            else
                reducer

        sums =
            List.foldl folder { upper = 0, lower = 0 } entries

        getBonus upper =
            if upper >= 63 then
                35

            else
                0
    in
    Html.table []
        [ Html.tr []
            [ Html.td [] [ Html.text "Sum Upper Part:" ]
            , Html.td [] [ Html.text (String.fromInt sums.upper) ]
            ]
        , Html.tr []
            [ Html.td [] [ Html.text "Bonus:" ]
            , Html.td [] [ Html.text (String.fromInt (getBonus sums.upper)) ]
            ]
        , Html.tr []
            [ Html.td [] [ Html.text "Sum Lower Part:" ]
            , Html.td [] [ Html.text (String.fromInt sums.lower) ]
            ]
        , Html.tr []
            [ Html.td [] [ Html.text "Sum All:" ]
            , Html.td [] [ Html.text (String.fromInt (sums.upper + sums.lower + getBonus sums.upper)) ]
            ]
        ]


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    { title = "Kniffel.elm"
    , body =
        [ Html.h1 [] [ Html.text "Kniffel.elm" ]
        , Html.div
            [ Attributes.id "kniffel_main" ]
            [ createDiceListHtml model.diceset
            , createRollCounterHtml model.controls
            , createButtonNextRoundHtml model.controls
            , createButtonRollHtml model.controls
            , createEntriesHtml model.entries
            , createSumsHtml model.entries
            ]
        ]
    }



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
