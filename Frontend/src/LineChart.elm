module LineChart exposing (viewChart)

import Axis
import Color
import Html exposing (Html)
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Time
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


getXScale : List Time.Posix -> ContinuousScale Time.Posix
getXScale dates =
    let
        minDate =
            List.sortWith (\a b -> compare (Time.posixToMillis a) (Time.posixToMillis b)) dates
                |> List.head
                |> Maybe.withDefault (Time.millisToPosix 0)

        maxDate =
            List.sortWith (\a b -> compare (Time.posixToMillis b) (Time.posixToMillis a)) dates
                |> List.head
                |> Maybe.withDefault (Time.millisToPosix 0)
    in
    Scale.time Time.utc ( 0, w - 2 * padding ) ( minDate, maxDate )


getYScale : List Float -> ContinuousScale Float
getYScale data =
    let
        minData =
            List.minimum data |> Maybe.withDefault 0

        maxData =
            List.maximum data |> Maybe.withDefault 2000
    in
    Scale.linear ( h - 2 * padding, 0 ) ( minData - 0.2 * (maxData - minData), maxData + 0.2 * (maxData - minData) )


xAxis : List ( Time.Posix, Float ) -> ContinuousScale Time.Posix -> Svg msg
xAxis model xScale =
    Axis.bottom [ Axis.tickCount (List.length model) ] xScale


yAxis : ContinuousScale Float -> Svg msg
yAxis yScale =
    Axis.left [ Axis.tickCount 5 ] yScale


transformToLineData : ContinuousScale Time.Posix -> ContinuousScale Float -> ( Time.Posix, Float ) -> Maybe ( Float, Float )
transformToLineData xScale yScale ( x, y ) =
    Just ( Scale.convert xScale x, Scale.convert yScale y )


line : ContinuousScale Time.Posix -> ContinuousScale Float -> List ( Time.Posix, Float ) -> Path
line xscale yscale data =
    List.map (transformToLineData xscale yscale) data
        |> Shape.line Shape.monotoneInXCurve


viewChart : List ( Time.Posix, Float ) -> Html msg
viewChart model =
    let
        xScale =
            getXScale <| List.map (\( time, _ ) -> time) model

        yScale =
            getYScale <| List.map (\( _, data ) -> data) model
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model xScale ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis yScale ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            [ Path.element (line xScale yScale model) [ stroke <| Paint <| Color.rgb 1 0 0, strokeWidth 3, fill PaintNone ]
            ]
        ]
