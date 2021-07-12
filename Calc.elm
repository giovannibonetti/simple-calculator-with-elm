module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type Operation
    = Add
    | Subtract
    | Multiply
    | Divide


type Model
    = OneOperand Int
    | TwoOperands Int Operation Int


init : Model
init =
    OneOperand 0



--UPDATE


type Msg
    = PressedClear
    | PressedAdd
    | PressedSubtract
    | PressedMultiply
    | PressedDivide
    | PressedEqual
    | PressedDigit Int


calculate : Operation -> Int -> Int -> Int
calculate operation =
    case operation of
        Add ->
            (+)

        Subtract ->
            (-)

        Multiply ->
            (*)

        Divide ->
            (//)


update : Msg -> Model -> Model
update msg model =
    case msg of
        PressedClear ->
            OneOperand 0

        PressedAdd ->
            case model of
                OneOperand first ->
                    TwoOperands first Add 0

                TwoOperands first operation second ->
                    TwoOperands (calculate operation first second) Add 0

        PressedSubtract ->
            case model of
                OneOperand first ->
                    TwoOperands first Subtract 0

                TwoOperands first operation second ->
                    TwoOperands (calculate operation first second) Subtract 0

        PressedMultiply ->
            case model of
                OneOperand first ->
                    TwoOperands first Multiply 0

                TwoOperands first operation second ->
                    TwoOperands (calculate operation first second) Multiply 0

        PressedDivide ->
            case model of
                OneOperand first ->
                    TwoOperands first Divide 0

                TwoOperands first operation second ->
                    TwoOperands (calculate operation first second) Divide 0

        PressedEqual ->
            case model of
                OneOperand first ->
                    OneOperand first

                TwoOperands first operation second ->
                    OneOperand (calculate operation first second)

        PressedDigit digit ->
            case model of
                OneOperand first ->
                    OneOperand (10 * first + digit)

                TwoOperands first operation second ->
                    TwoOperands first operation (10 * second + digit)



--VIEW


calculatorButton : Msg -> String -> Html Msg
calculatorButton msg buttonText =
    button
        [ class "button", onClick msg ]
        [ span [] [ text buttonText ] ]


calculatorButtonWide : Msg -> String -> Html Msg
calculatorButtonWide msg buttonText =
    button
        [ class "button wide", onClick msg ]
        [ span [] [ text buttonText ] ]


showCurrentOperand : Model -> String
showCurrentOperand model =
    case model of
        OneOperand first ->
            String.fromInt first

        TwoOperands _ _ second ->
            String.fromInt second


stylesheet : Html a
stylesheet =
    node "link"
        [ attribute "Rel" "stylesheet"
        , attribute "property" "stylesheet"
        , attribute "href" "styles.css"
        ]
        []


view : Model -> Html Msg
view model =
    div [ class "tudo" ]
        [ h1 [] [ text "Simple Calculator" ]
        , div [ class "calculator" ]
            [ stylesheet
            , div [ class "row" ]
                [ div [ class "col-xs-12" ]
                    [ div [ class "display" ]
                        [ div [ class "display-text" ]
                            [ text (showCurrentOperand model) ]
                        ]
                    , div [ class "buttons" ]
                        [ calculatorButton (PressedDigit 7) "7"
                        , calculatorButton (PressedDigit 8) "8"
                        , calculatorButton (PressedDigit 9) "9"
                        , calculatorButton PressedDivide "÷"
                        , br [] []
                        , calculatorButton (PressedDigit 4) "4"
                        , calculatorButton (PressedDigit 5) "5"
                        , calculatorButton (PressedDigit 6) "6"
                        , calculatorButton PressedMultiply "X"
                        , br [] []
                        , calculatorButton (PressedDigit 1) "1"
                        , calculatorButton (PressedDigit 2) "2"
                        , calculatorButton (PressedDigit 3) "3"
                        , calculatorButton PressedAdd "+"
                        , br [] []
                        , calculatorButtonWide PressedClear "c"
                        , calculatorButton (PressedDigit 0) "0"
                        , calculatorButton PressedEqual "="
                        , calculatorButton PressedSubtract "-"
                        ]
                    ]
                ]
            ]
        ]
