module Calc exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


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
    = PushClear
    | PushOperation Operation
    | PushEqual
    | PushDigit Int


calc : Operation -> Int -> Int -> Int
calc operation =
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
        PushClear ->
            OneOperand 0

        PushOperation newOp ->
            case model of
                OneOperand x ->
                    TwoOperands x newOp 0

                TwoOperands x oldOp y ->
                    TwoOperands (calc oldOp x y) newOp 0

        PushEqual ->
            case model of
                OneOperand x ->
                    OneOperand x

                TwoOperands x op y ->
                    OneOperand (calc op x y)

        PushDigit digit ->
            case model of
                OneOperand x ->
                    OneOperand (10 * x + digit)

                TwoOperands x op y ->
                    TwoOperands x op (10 * y + digit)



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
                        [ calculatorButton (PushDigit 7) "7"
                        , calculatorButton (PushDigit 8) "8"
                        , calculatorButton (PushDigit 9) "9"
                        , calculatorButton (PushOperation Divide) "÷"
                        , br [] []
                        , calculatorButton (PushDigit 4) "4"
                        , calculatorButton (PushDigit 5) "5"
                        , calculatorButton (PushDigit 6) "6"
                        , calculatorButton (PushOperation Multiply) "X"
                        , br [] []
                        , calculatorButton (PushDigit 1) "1"
                        , calculatorButton (PushDigit 2) "2"
                        , calculatorButton (PushDigit 3) "3"
                        , calculatorButton (PushOperation Add) "+"
                        , br [] []
                        , calculatorButtonWide PushClear "c"
                        , calculatorButton (PushDigit 0) "0"
                        , calculatorButton PushEqual "="
                        , calculatorButton (PushOperation Subtract) "-"
                        ]
                    ]
                ]
            ]
        ]
