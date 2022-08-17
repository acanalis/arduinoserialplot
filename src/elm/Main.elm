module Main exposing (..)

import Browser
import Chart as C
import Chart.Attributes as CA
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
    { offset : Float
    , points : List Point
    }


type alias Model =
    { currentpoint : Point
    , series : List Series
    , status : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( Model (Point 0 0) [] "inicio"
    , Cmd.none
    )


type Msg
    = RequestNewpoint
    | ServerResponse (Result Http.Error ServerMsg)
    | NewSeries
    | ResetSeries


type alias ServerMsg =
    { serialisonline : Bool
    , newpoints : Maybe (List Point)
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewSeries ->
            ( { model | series = List.append model.series [ Series model.currentpoint.x [] ] }
            , Cmd.none
            )

        ResetSeries ->
            ( { model | series = List.indexedMap (\i s -> if i==List.length model.series-1 then Series model.currentpoint.x [] else s) model.series }
            , Cmd.none
            )

        ServerResponse (Err err) ->
            ( { model | status = "error:" ++ errorToString err }, Cmd.none )

        ServerResponse (Ok servermsg) ->
            case servermsg.newpoints of
                Nothing ->
                    ( { model | status = "no se recibieron nuevos puntos" }, Cmd.none )

                Just pointlist ->
                    ( updatePoints model pointlist
                    , Cmd.none
                    )

        RequestNewpoint ->
            ( model
            , Http.get { url = "http://localhost:8080/newpoints", expect = Http.expectJson ServerResponse serverResponseDecoder }
            )


updatePoints : Model -> List Point -> Model
updatePoints model newpoints =
    let
        last =
            List.length model.series
    in
    { model
        | series =
            List.indexedMap
                (\i s ->
                    if i == last - 1 then
                        { s | points = s.points ++ List.map (\p -> { p | x = p.x - s.offset }) newpoints }

                    else
                        s
                )
                model.series
        , currentpoint = List.head (List.reverse newpoints) |> withDefault model.currentpoint
    }


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
        ]


viewSeries : Model -> List (C.Element Point msg)
viewSeries model =
    let
        last =
            List.length model.series
    in
    List.indexedMap
        (\i serie ->
            C.series .x
                [ C.interpolated .y
                    (if i == last - 1 then
                        [ CA.width 2, defaultColors i ]

                     else
                        [ CA.width 0.5, defaultColors i ]
                    )
                    []
                ]
                serie.points
        )
        model.series


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
