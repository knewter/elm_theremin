port module Main exposing (..)

import Html exposing (Html, div, button, text, h1, section)
import Html.Events exposing (onClick)
import Html.App as App
import Window
import Task exposing (andThen)
import Mouse


type alias Model =
    { gainValue : Float
    , frequencyValue : Float
    , windowWidth : Int
    , windowHeight : Int
    , visualizationData : List Int
    }


type Msg
    = IncrementGain
    | DecrementGain
    | IncrementFrequency
    | DecrementFrequency
    | UpdateDimensions { width : Int, height : Int }
    | UpdateMouse ( Int, Int )
    | Visualization (List Int)
    | NoOp


model =
    { gainValue = 0.001
    , frequencyValue = 3000
    , windowWidth = 100
    , windowHeight = 100
    , visualizationData = []
    }


getInitialWindowSize : Cmd Msg
getInitialWindowSize =
    Task.perform (\_ -> NoOp) UpdateDimensions Window.size


main =
    App.program
        { init = ( model, getInitialWindowSize )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes UpdateDimensions
        , Mouse.moves (\{ x, y } -> UpdateMouse ( x, y ))
        , visualization Visualization
        ]


view : Model -> Html Msg
view model =
    div []
        [ section []
            [ h1 [] [ text "Gain" ]
            , button [ onClick DecrementGain ] [ text "-" ]
            , div [] [ text (toString model.gainValue) ]
            , button [ onClick IncrementGain ] [ text "+" ]
            ]
        , section []
            [ h1 [] [ text "Frequency" ]
            , button [ onClick DecrementFrequency ] [ text "-" ]
            , div [] [ text (toString model.frequencyValue) ]
            , button [ onClick IncrementFrequency ] [ text "+" ]
            ]
        , section []
            [ h1 [] [ text "Dimensions" ]
            , div [] [ text (toString model.windowHeight) ]
            , div [] [ text (toString model.windowWidth) ]
            ]
        , section []
            [ h1 [] [ text "Visualization Data" ]
            , div [] [ text (toString model.visualizationData) ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncrementGain ->
            let
                newModel =
                    { model | gainValue = model.gainValue + 0.001 }
            in
                ( newModel, audio newModel )

        DecrementGain ->
            let
                newModel =
                    { model | gainValue = model.gainValue - 0.001 }
            in
                ( newModel, audio newModel )

        IncrementFrequency ->
            let
                newModel =
                    { model | frequencyValue = model.frequencyValue + 100 }
            in
                ( newModel, audio newModel )

        DecrementFrequency ->
            let
                newModel =
                    { model | frequencyValue = model.frequencyValue - 100 }
            in
                ( newModel, audio newModel )

        UpdateDimensions { width, height } ->
            let
                newModel =
                    { model | windowWidth = width, windowHeight = height }
            in
                ( newModel, audio newModel )

        UpdateMouse ( x, y ) ->
            let
                -- gain is the percentage you are across the screen, from left to right, mapped from 0 to 0.03
                newGain =
                    ((toFloat x) / (toFloat model.windowWidth)) * 0.03

                -- frequency is the percentage you are vertically down the screen, mapped from 0 to 6000
                newFrequency =
                    ((toFloat y) / (toFloat model.windowHeight)) * 6000.0

                newModel =
                    { model | frequencyValue = newFrequency, gainValue = newGain }
            in
                ( newModel, audio newModel )

        Visualization data ->
            ( { model | visualizationData = data }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


port audio : Model -> Cmd msg


port visualization : (List Int -> msg) -> Sub msg
