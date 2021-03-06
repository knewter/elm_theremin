port module Main exposing (..)

import Html exposing (Html, div, button, text, h1, section, node)
import Html.Attributes exposing (class, type')
import Html.Events exposing (onClick)
import Html.App as App
import Window
import Task exposing (andThen)
import Mouse
import Element exposing (Element)
import Collage exposing (collage, path, traced, solid, move, alpha, Form)
import Color exposing (..)


type alias Model =
    { gainValue : Float
    , frequencyValue : Float
    , windowWidth : Int
    , windowHeight : Int
    , visualizationPaths : List Form
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
    , visualizationPaths = []
    }


pastVisualizationCount : Int
pastVisualizationCount =
    10


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
        [ styles
        , section []
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
        , div [ class "visualization" ]
            [ (visualizationGraph model) |> Element.toHtml ]
        ]


styles : Html Msg
styles =
    node "style"
        [ type' "text/css" ]
        [ text ".visualization { position: absolute; top: 0; left: 0; }" ]


visualizationGraph : Model -> Element
visualizationGraph model =
    let
        applyAlpha : Int -> Form -> Form
        applyAlpha count path =
            case count of
                1 ->
                    path

                _ ->
                    path |> alpha 0.1
    in
        collage model.windowWidth
            model.windowHeight
            (List.indexedMap applyAlpha model.visualizationPaths)


visualizationGraphForDatum : Int -> Int -> List Int -> Form
visualizationGraphForDatum windowWidth windowHeight datum =
    let
        points =
            toPoints windowWidth windowHeight datum
    in
        path points
            |> traced (solid red)
            |> move ( (toFloat windowWidth) / -2, (toFloat windowHeight) / -2 )


toPoints : Int -> Int -> List Int -> List ( Float, Float )
toPoints windowWidth windowHeight datum =
    let
        -- The width of each slice is the window width divided by the number of
        -- data points we have.
        sliceWidth =
            (toFloat windowWidth) / (toFloat (List.length datum))

        -- Turning a given piece of data into a point requires knowing its index in
        -- the list.
        indexedDatumToPoint n internalDatum =
            let
                -- Its y coordinate should be its percentage (out of 128 total) times
                -- the window height, divided by 2.
                v =
                    (toFloat internalDatum) / 128

                y =
                    (v * (toFloat windowHeight)) / 2

                -- And its x coordinate is its percentage of the total data set times
                -- the slice width.
                x =
                    sliceWidth * (toFloat n)
            in
                ( x, y )
    in
        datum
            |> List.indexedMap indexedDatumToPoint


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncrementGain ->
            let
                newModel =
                    { model | gainValue = model.gainValue + 0.001 }
            in
                ( newModel, sendAudio newModel )

        DecrementGain ->
            let
                newModel =
                    { model | gainValue = model.gainValue - 0.001 }
            in
                ( newModel, sendAudio newModel )

        IncrementFrequency ->
            let
                newModel =
                    { model | frequencyValue = model.frequencyValue + 100 }
            in
                ( newModel, sendAudio newModel )

        DecrementFrequency ->
            let
                newModel =
                    { model | frequencyValue = model.frequencyValue - 100 }
            in
                ( newModel, sendAudio newModel )

        UpdateDimensions { width, height } ->
            let
                newModel =
                    { model | windowWidth = width, windowHeight = height }
            in
                ( newModel, sendAudio newModel )

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
                ( newModel, sendAudio newModel )

        Visualization data ->
            ( (updateVisualizationData data model), Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateVisualizationData : List Int -> Model -> Model
updateVisualizationData data model =
    let
        newVisualizationPath =
            visualizationGraphForDatum model.windowWidth model.windowHeight data

        newVisualizationPaths =
            newVisualizationPath :: model.visualizationPaths
    in
        { model | visualizationPaths = List.take 10 newVisualizationPaths }


sendAudio : Model -> Cmd msg
sendAudio model =
    audio { frequencyValue = model.frequencyValue, gainValue = model.gainValue }


port audio : { frequencyValue : Float, gainValue : Float } -> Cmd msg


port visualization : (List Int -> msg) -> Sub msg
