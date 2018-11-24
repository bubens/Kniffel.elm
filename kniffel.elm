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
            let
                mapper =
                    \dice -> Dice.roll (1 + dice.face % 6) dice

                newDiceset =
                    Array.map mapper model.diceset
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
