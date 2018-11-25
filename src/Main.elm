module Main exposing (..)

import Html
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes
import Dice exposing (Dice)
import Dice
import List
import Array exposing (Array)
import Array
import Random


-- MODEL


type alias Diceset =
    Array Dice


type alias Model =
    { diceset : Diceset }



-- INIT


init : ( Model, Cmd Msg )
init =
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


createListItem : Int -> Dice -> Html Msg
createListItem index dice =
    Html.li
        [ Events.onClick (DiceClicked index)
        ]
        [ Dice.toSVG dice
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
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
