module Main exposing (Diceset, Model, Msg(..), createDiceList, createListItem, init, main, rollDiceset, subscriptions, update, view)

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


type alias Model =
    { diceset : Diceset }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( Array.fromList [ 1, 1, 1, 1, 1 ]
        |> Array.map (\x -> Dice.create x)
        |> Model
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


createListItem : Int -> Dice -> Html Msg
createListItem index dice =
    let
        width =
            getDiceWidth 100 dice.held
    in
    Html.li
        [ Events.onClick (DiceClicked index)
        ]
        [ Dice.toSVG width dice
        ]


createDiceList : Diceset -> Html Msg
createDiceList diceset =
    diceset
        |> Array.indexedMap createListItem
        |> Array.toList
        |> Html.ul []


view : Model -> Html Msg
view model =
    Html.div
        [ Attributes.id "main" ]
        [ createDiceList model.diceset
        , Html.button
            [ Events.onClick ButtonRollClicked
            ]
            [ Html.text "Roll!"
            ]
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
