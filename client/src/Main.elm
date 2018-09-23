module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



---- MODEL ----


init : ( Model, Cmd Msg )
init =
    ( { buttons =
            [ Button Enabled "" 1 1 DarkGreen -- row1
            , Button Enabled "" 1 1 DarkGreen
            , Button Enabled "" 1 1 DarkGreen
            , Button Enabled "" 1 1 DarkGreen
            , Button Enabled "" 1 1 DarkGreen
            , Button Enabled "" 1 1 DarkGreen
            , Button Enabled "" 1 1 DarkGreen
            , Button Enabled "" 1 1 DarkGreen
            , Button Enabled "" 1 1 DarkGreen
            , Button Enabled "" 1 1 DarkGreen
            , Button Enabled "" 2 2 Green -- row2
            , Button Enabled "" 2 2 Green
            , Button Enabled "" 2 2 Green
            , Button Enabled "" 2 2 Green
            , Button Enabled "" 2 2 Green
            , Button Enabled "" 2 2 Green
            , Button Enabled "7" 2 2 Beige
            , Button Enabled "8" 2 2 Beige
            , Button Enabled "9" 2 2 Beige
            , Button Enabled "DEL" 2 4 Pink
            , Button Enabled "" 3 3 Green -- row3
            , Button Enabled "" 3 3 Green
            , Button Enabled "" 3 3 Green
            , Button Enabled "" 3 3 Green
            , Button Enabled "" 3 3 Green
            , Button Enabled "" 3 3 Green
            , Button Enabled "4" 3 3 Beige
            , Button Enabled "5" 3 3 Beige
            , Button Enabled "6" 3 3 Beige
            , Button Enabled "" 4 4 Green -- row4
            , Button Enabled "" 4 4 Green
            , Button Enabled "" 4 4 Green
            , Button Enabled "" 4 4 Green
            , Button Enabled "" 4 4 Green
            , Button Enabled "" 4 4 Green
            , Button Enabled "1" 4 4 Beige
            , Button Enabled "2" 4 4 Beige
            , Button Enabled "3" 4 4 Beige
            , Button Enabled "EXC" 4 6 Pink
            , Button Enabled "" 5 5 Green --row5
            , Button Enabled "" 5 5 Green
            , Button Enabled "" 5 5 Green
            , Button Enabled "" 5 5 Green
            , Button Enabled "" 5 5 Green
            , Button Enabled "" 5 5 Green
            , Button Enabled "HAND OFF" 5 5 Purple
            , Button Enabled "0" 5 5 Beige
            , Button Enabled "CLEAR" 5 5 Pink
            , Button Enabled "Q" 6 6 Beige --row6
            , Button Enabled "W" 6 6 Beige
            , Button Enabled "E" 6 6 Beige
            , Button Enabled "R" 6 6 Beige
            , Button Enabled "T" 6 6 Beige
            , Button Enabled "Y" 6 6 Beige
            , Button Enabled "U" 6 6 Beige
            , Button Enabled "I" 6 6 Beige
            , Button Enabled "O" 6 6 Beige
            , Button Enabled "P" 6 6 Beige
            , Button Enabled "A" 7 7 Beige --row7
            , Button Enabled "S" 7 7 Beige
            , Button Enabled "D" 7 7 Beige
            , Button Enabled "F" 7 7 Beige
            , Button Enabled "G" 7 7 Beige
            , Button Enabled "H" 7 7 Beige
            , Button Enabled "J" 7 7 Beige
            , Button Enabled "K" 7 7 Beige
            , Button Enabled "L" 7 7 Beige
            , Button Enabled "DEL CHAR" 7 7 Pink
            , Button Disabled "FDD" 8 8 Pink --row7
            , Button Enabled "X" 8 8 Beige
            , Button Enabled "C" 8 8 Beige
            , Button Enabled "V" 8 8 Beige
            , Button Enabled "B" 8 8 Beige
            , Button Enabled "N" 8 8 Beige
            , Button Enabled "M" 8 8 Beige
            , Button Enabled "K" 8 8 Beige
            , Button Enabled "SPACE" 8 8 Pink
            , Button Disabled "-" 8 8 Pink
            ]
      }
    , Cmd.none
    )


type alias Model =
    { buttons : List Button }


type alias Button =
    { state : ButtonState
    , text : String
    , rowStart : Int
    , rowEnd : Int
    , color : Color
    }


type ButtonState
    = Enabled
    | Selected
    | Disabled
    | Pressed


type Color
    = Pink
    | Green
    | Purple
    | DarkGreen
    | Beige



---- UPDATE ----


type Msg
    = ButtonPressed Int
    | ButtonReleased Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ButtonPressed idx ->
            ( setBtnState model idx Pressed, Cmd.none )

        ButtonReleased idx ->
            ( setBtnState model idx Enabled, Cmd.none )


setBtnState : Model -> Int -> ButtonState -> Model
setBtnState model btnIdx buttonState =
    let
        newButtons =
            List.indexedMap
                (\i a ->
                    if btnIdx == i then
                        { a | state = buttonState }

                    else if a.state /= Disabled then
                        { a | state = Enabled }

                    else
                        a
                )
                model.buttons
    in
    { model | buttons = newButtons }



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "grid-container" ]
        (buttonGrid model.buttons)


buttonGrid : List Button -> List (Html Msg)
buttonGrid buttonList =
    buttonList |> (List.indexedMap <| createButton)


createButton : Int -> Button -> Html Msg
createButton idx btn =
    div
        [ class "button"
        , onMouseDown (ButtonPressed idx)
        , onMouseUp (ButtonReleased idx)
        , style "grid-row-start" (String.fromInt btn.rowStart)
        , style "grid-row-end" (String.fromInt btn.rowEnd)
        , classList
            [ ( "pressed", btn.state == Pressed )
            , ( "disabled", btn.state == Disabled )
            , btn.color |> colorToClass
            ]
        ]
        [ span [ class "buttonText" ] [ text btn.text ] ]


colorToClass : Color -> ( String, Bool )
colorToClass color =
    case color of
        Pink ->
            ( "pink", True )

        Green ->
            ( "green", True )

        Purple ->
            ( "purple", True )

        DarkGreen ->
            ( "darkGreen", True )

        Beige ->
            ( "beige", True )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
