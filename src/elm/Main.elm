module Main exposing (..)

import Browser
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Dec
import List
import Maybe exposing (withDefault)
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Point =
    { x : Float, y : Float }


type alias Series =
    { offset : Maybe Float
    , points : List Point
    , data : SeriesData
    }


type alias SeriesData =
    { name : String
    , marker : { x : Float, y : Maybe Float }
    }


type alias Model =
    { currentpoint : Point
    , currentseries : Series
    , series : List Series
    , status : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( Model (Point 0 0)
        { offset = Nothing
        , points = []
        , data = {name = "Serie 0", marker = {x = 0, y = Nothing}}
        }
        []
        "inicio"
    , Cmd.none
    )


type Msg
    = RequestNewpoint
    | ServerResponse (Result Http.Error ServerMsg)
    | NewSeries
    | ResetSeries
    | DataNameMsg String
    | DataMoveMarkerMsg CE.Point


type alias ServerMsg =
    { serialisonline : Bool
    , newpoints : Maybe (List Point)
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewSeries ->
            ( { model
                | currentseries =
                    { offset = Just model.currentpoint.x
                    , points = []
                    , data = { name = "Serie N°" ++ String.fromInt (List.length model.series + 1), marker = { x = 0, y = Nothing } }
                    }
                , series = model.currentseries :: model.series
              }
            , Cmd.none
            )

        ResetSeries ->
            ( { model
                | currentseries =
                    { offset = Just model.currentpoint.x
                    , points = []
                    , data = { name = model.currentseries.data.name, marker = { x = 0, y = Nothing } }
                    }
              }
            , Cmd.none
            )

        ServerResponse (Err err) ->
            ( { model | status = "error:" ++ errorToString err }, Cmd.none )

        ServerResponse (Ok servermsg) ->
            case servermsg.newpoints of
                Nothing ->
                    ( { model | status = "no se recibieron nuevos puntos" }, Cmd.none )

                Just newpoints ->
                    ( { model
                        | currentseries = updatePoints model.currentseries newpoints
                        , currentpoint = List.head (List.reverse newpoints) |> withDefault model.currentpoint
                      }
                    , Cmd.none
                    )

        RequestNewpoint ->
            ( model
            , Http.get { url = "http://localhost:8080/newpoints", expect = Http.expectJson ServerResponse serverResponseDecoder }
            )

        DataNameMsg newname ->
            ( { model | currentseries = updateDataName model.currentseries newname }
            , Cmd.none
            )

        DataMoveMarkerMsg point ->
            ( { model | currentseries = updateDataMarker model.currentseries point }, Cmd.none )


updatePoints : Series -> List Point -> Series
updatePoints series newpoints =
    case ( series.offset, newpoints ) of
        ( Just offset, _ ) ->
            { series | points = series.points ++ List.map (\p -> { p | x = p.x - offset }) newpoints }

        ( Nothing, h :: _ ) ->
            { series | offset = Just h.x, points = List.map (\p -> { p | x = p.x - h.x }) newpoints }

        ( _, _ ) ->
            series


updateDataName : Series -> String -> Series
updateDataName series name =
    { series | data = { name = name, marker = series.data.marker } }


updateDataMarker : Series -> CE.Point -> Series
updateDataMarker series point =
    { series | data = { name = series.data.name, marker = { x = point.x, y = interp series.points point.x } } }


interp : List Point -> Float -> Maybe Float
interp points marker =
    case points of
        a :: b :: c ->
            if a.x < marker && marker < b.x then
                Just <| a.y + (b.y - a.y) * (marker - a.x) / (b.x - a.x)

            else
                interp (b :: c) marker

        _ ->
            Nothing


serverResponseDecoder : Dec.Decoder ServerMsg
serverResponseDecoder =
    Dec.map2 ServerMsg
        (Dec.field "SerialIsOnline" Dec.bool)
        (Dec.field "Newpoints" <| Dec.nullable <| Dec.list <| Dec.map2 Point (Dec.field "x" Dec.float) (Dec.field "y" Dec.float))



-- SUBSCRIPTIONS
-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 2000 (\_ -> RequestNewpoint)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ id "chartcontainer" ]
            [ C.chart
                [ CA.height 360
                , CA.width 480
                , CA.range
                    [ CA.lowest 0 CA.orLower
                    , CA.highest 1000 CA.orHigher
                    ]
                , CA.domain
                    [ CA.lowest 0 CA.orLower
                    , CA.highest 30 CA.orHigher
                    ]
                , CE.onMouseDown DataMoveMarkerMsg CE.getCoords
                ]
                ([ C.xAxis []
                 , C.xTicks []
                 , C.xLabels []
                 , C.yAxis []
                 , C.yTicks []
                 , C.yLabels []
                 ]
                    ++ viewSeries model
                )
            ]
        , div [ id "controls" ]
            [ button [ onClick NewSeries ] [ text "Nueva Serie" ]
            , button [ onClick ResetSeries ] [ text "Resetear Serie" ]
            ]
        , text model.status
        , text
            ("Marker Value: x ="
                ++ String.fromFloat model.currentseries.data.marker.x
                ++ ", y ="
                ++ (case model.currentseries.data.marker.y of
                        Just value ->
                            String.fromFloat value

                        Nothing ->
                            "Out of range"
                   )
            )
        , input [ type_ "text", placeholder "Name", value model.currentseries.data.name, onInput DataNameMsg ] []
        ]


viewSeries : Model -> List (C.Element Point msg)
viewSeries model =
    List.append
        (List.indexedMap
            (\i serie ->
                C.series .x
                    [ C.interpolated .y
                        [ CA.width 0.5, defaultColors (List.length model.series - i) ]
                        []
                    ]
                    serie.points
            )
            model.series
        )
        [ C.line
            [ CA.x1 model.currentseries.data.marker.x
            , CA.x2 model.currentseries.data.marker.x
            , CA.y1 0
            , CA.y2 30
            , CA.width 2
            , CA.color "#000000"
            ]
        , C.series .x
            [ C.interpolated .y [ CA.width 2, CA.color CA.red ] [] ]
            model.currentseries.points
        ]


defaultColors : Int -> CA.Attribute { a | color : String }
defaultColors index =
    let
        color =
            case modBy 7 index of
                0 ->
                    CA.brown

                1 ->
                    CA.purple

                2 ->
                    CA.pink

                3 ->
                    CA.blue

                4 ->
                    CA.green

                5 ->
                    CA.yellow

                6 ->
                    CA.orange

                _ ->
                    "#000000"
    in
    CA.color color


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Http.Timeout ->
            "Unable to reach the server, try again"

        Http.NetworkError ->
            "Unable to reach the server, check your network connection"

        Http.BadStatus 500 ->
            "The server had a problem, try again later"

        Http.BadStatus 400 ->
            "Verify your information and try again"

        Http.BadStatus _ ->
            "Unknown error"

        Http.BadBody errorMessage ->
            errorMessage
