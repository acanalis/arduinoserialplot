module Main exposing (..)

import Browser
import Chart as C
import Chart.Attributes as CA
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Dec
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


type alias Model =
    { points : List Point
    , cmd : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( Model [] ""
    , Cmd.none
    )


type Msg
    = RequestNewpoint
    | ServerResponse (Result Http.Error ServerMsg)


type alias ServerMsg =
    { serialisonline : Bool
    , newpoints : Maybe (List Point)
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ServerResponse (Err err) ->
            ( { model | cmd = "error" ++ Debug.toString err }, Cmd.none )

        ServerResponse (Ok servermsg) ->
            case servermsg.newpoints of
                Nothing ->
                    ( { model | cmd = "no se recibieron nuevos puntos" }, Cmd.none )

                Just pointlist ->
                    ( { model | points = model.points ++ pointlist }
                    , Cmd.none
                    )

        RequestNewpoint ->
            ( model
            , Http.get { url = "http://localhost:8080/newpoints", expect = Http.expectJson ServerResponse serverResponseDecoder }
            )


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
    div
        [ style "height" "400px"
        , style "width" "600px"
        , style "margin" "50px"
        ]
        [ text model.cmd
        , C.chart
            [ CA.height 300
            , CA.width 300
            ]
            [ C.xAxis []
            , C.xTicks []
            , C.xLabels []
            , C.yAxis []
            , C.yTicks []
            , C.yLabels []
            , C.series .x
                [ C.scatter .y [] ]
                model.points
            ]
        ]
