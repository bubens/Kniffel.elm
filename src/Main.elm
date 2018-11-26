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
    }



-- INIT


createEntry : String -> EntryType -> Entry
createEntry name type_ =
    Entry name type_ -1 False


initDiceset : List Int -> Diceset
initDiceset initvals =
    Array.fromList initvals
        |> Array.map (\x -> Dice.create x)


initEntries : List Entry
initEntries =
    [ createEntry "Ones" (Sum 1)
    , createEntry "Twos" (Sum 2)
    , createEntry "Threes" (Sum 3)
    , createEntry "Fours" (Sum 4)
    , createEntry "Fives" (Sum 5)
    , createEntry "Six" (Sum 6)
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


rollDiceset : Random.Generator (List Int)
rollDiceset =
    Random.int 1 6
        |> Random.list 6


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DiceClicked index ->
            let
                mapper =
                    \i ->
                        \dice ->
                            if i == index then
                                Dice.hold (not dice.held) dice

                            else
                                dice

                newDiceset =
                    Array.indexedMap mapper model.diceset
            in
            ( { model | diceset = newDiceset }, Cmd.none )

        ButtonRollClicked ->
            ( model, Random.generate DiceRolled rollDiceset )

        DiceRolled result ->
            let
                mapper =
                    \x -> \dice -> Dice.roll x dice

                newDiceset =
                    Array.toList model.diceset
                        |> List.map2 mapper result
                        |> Array.fromList
            in
            ( { model | diceset = newDiceset }, Cmd.none )



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


createEntriesHtml : List Entry -> Html Msg
createEntriesHtml entries =
    Html.table [] []


view : Model -> Html Msg
view model =
    Html.div
        [ Attributes.id "main" ]
        [ createDiceListHtml model.diceset
        , Html.button
            [ Events.onClick ButtonRollClicked
            , Attributes.style "clear" "both"
            , Attributes.style "display" "block"
            ]
            [ Html.text "Roll!"
            ]
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
