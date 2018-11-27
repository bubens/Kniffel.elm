module Main exposing (Diceset, Model, Msg(..), createDiceListHtml, createListItemHtml, init, main, rollDiceset, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Dice exposing (Dice)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import List
import Random



-- MODEL


type alias Diceset =
    Array Dice


type EntryType
    = Sum Int
    | Predefined Int


type alias Entry =
    { name : String
    , entryType : EntryType
    , value : Int
    , entered : Bool
    }


type alias Model =
    { diceset : Diceset
    , entries : List Entry
    , sums : List Entry
    }



-- INIT


initEntry : String -> EntryType -> Int -> Bool -> Entry
initEntry name type_ value held =
    Entry name type_ value held


initDiceset : List Int -> Diceset
initDiceset initvals =
    Array.fromList initvals
        |> Array.map (\x -> Dice.create x)


initEntries : List Entry
initEntries =
    [ initEntry "Ones" (Sum 1) -1 False
    , initEntry "Twos" (Sum 2) -1 False
    , initEntry "Threes" (Sum 3) -1 False
    , initEntry "Fours" (Sum 4) -1 False
    , initEntry "Fives" (Sum 5) -1 False
    , initEntry "Six" (Sum 6) -1 False
    , initEntry "All 3" (Sum 0) -1 False
    , initEntry "All 4" (Sum 0) -1 False
    , initEntry "Full House" (Predefined 35) -1 False
    , initEntry "Small Straight" (Predefined 30) -1 False
    , initEntry "Large Straight" (Predefined 40) -1 False
    , initEntry "Yahtzee" (Predefined 50) -1 False
    , initEntry "Chance" (Sum 0) -1 False
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (initDiceset [ 1, 1, 1, 1, 1 ])
        initEntries
    , Cmd.none
    )



-- UPDATE


type Msg
    = DiceClicked Int
    | ButtonRollClicked
    | DiceRolled (List Int)
    | EntryClicked String


rollDiceset : Random.Generator (List Int)
rollDiceset =
    Random.int 1 6
        |> Random.list 6


updateDiceClicked : Int -> Model -> ( Model, Cmd Msg )
updateDiceClicked index model =
    let
        mapper i dice =
            if i == index then
                Dice.hold (not dice.held) dice

            else
                dice

        newDiceset =
            Array.indexedMap mapper model.diceset
    in
    ( { model | diceset = newDiceset }, Cmd.none )


updateDiceRolled : List Int -> Model -> ( Model, Cmd Msg )
updateDiceRolled result model =
    let
        mapper x dice =
            Dice.roll x dice

        newDiceset =
            Array.toList model.diceset
                |> List.map2 mapper result
                |> Array.fromList
    in
    ( { model | diceset = newDiceset }, Cmd.none )


updateEntryClicked : String -> Model -> ( Model, Cmd Msg )
updateEntryClicked name model =
    let
        mapper entry =
            if entry.name == name then
                initEntry entry.name entry.entryType 999 True

            else
                entry

        newEntries =
            List.map mapper model.entries
    in
    ( { model | entries = newEntries }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DiceClicked index ->
            updateDiceClicked index model

        ButtonRollClicked ->
            ( model, Random.generate DiceRolled rollDiceset )

        DiceRolled result ->
            updateDiceRolled result model

        EntryClicked name ->
            updateEntryClicked name model



-- VIEW


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
        [ Events.onClick (DiceClicked index)
        , Attributes.style "float" "left"
        ]
        [ Dice.toSVG width dice
        ]


createDiceListHtml : Diceset -> Html Msg
createDiceListHtml diceset =
    diceset
        |> Array.indexedMap createListItemHtml
        |> Array.toList
        |> Html.ul [ Attributes.style "list-style" "none" ]



--Attributes.style


getEntryValue : Int -> String
getEntryValue value =
    if value < 0 then
        " "

    else
        String.fromInt value


createEntriesRowHtml : Entry -> Html Msg
createEntriesRowHtml entry =
    Html.tr
        [ Events.onClick (EntryClicked entry.name)
        ]
        [ Html.td [] [ Html.text (entry.name ++ ":") ]
        , Html.td [] [ Html.text (getEntryValue entry.value) ]
        ]


createEntriesHtml : List Entry -> Html Msg
createEntriesHtml entries =
    entries
        |> List.map createEntriesRowHtml
        |> Html.table []


buttonRollHtml : Html Msg
buttonRollHtml =
    Html.button
        [ Events.onClick ButtonRollClicked
        , Attributes.style "clear" "both"
        , Attributes.style "display" "block"
        ]
        [ Html.text "Roll!"
        ]


view : Model -> Html Msg
view model =
    Html.div
        [ Attributes.id "main" ]
        [ createDiceListHtml model.diceset
        , buttonRollHtml
        , createEntriesHtml model.entries
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
