module Main exposing (..)

import Html exposing (Html, text, div, h1, img, ul, li)
import Html.Attributes exposing (src, style, id, class)
import Json.Decode exposing (..)
import Html.Events exposing (onClick, on)
import Http
import Debug
import ISO8601
import Svg
import Svg.Attributes
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Coordinate as Coordinate
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Legends as Legends
import LineChart.Line as Line
import LineChart.Line as Line


---- MODEL ----


type alias Model =
    { flags : Flags
    , stats : List Cid
    , dataDisplay : String
    , hovered : Maybe Cid
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags
      , stats = destruktor flags.chart_data
      , dataDisplay = "none"
      , hovered = Nothing
      }
    , Cmd.none
    )


type alias Flags =
    { analyteid : Int
    , chart_data : String
    }


type alias MyData =
    { vals : Vals }


type alias Vals =
    { stats : Stats
    , qcresults : List Cid
    }


type alias Stats =
    { nominal : Float
    , mean : Float
    , deviation : Float
    }


type alias Cid =
    { id : Int
    , c : Float
    , d : ISO8601.Time
    }


type alias Point =
    { x : Float, y : Float }



-- DECODERS
-- repl use:       decodeString Main.decodeMyData Main.exampleData


decodeMyData : Decoder MyData
decodeMyData =
    map MyData (field "vals" decodeVals)


decodeVals : Decoder Vals
decodeVals =
    (map2 Vals (field "stats" decodeStats) (field "qcresults" decodeQcresults))


decodeStats : Decoder Stats
decodeStats =
    map3 Stats (field "nominal" float) (field "mean" float) (field "deviation" float)


decodeQcresults : Decoder (List Cid)
decodeQcresults =
    list decodeCid


decodeCid : Decoder Cid
decodeCid =
    map3 Cid (field "id" int) (field "c" float) (field "d" decodeTimeStamp)


decodeTimeStamp : Decoder ISO8601.Time
decodeTimeStamp =
    string
        |> andThen
            (\val ->
                (case (ISO8601.fromString val) of
                    Ok ts ->
                        Json.Decode.succeed ts

                    Err errmsg ->
                        Json.Decode.fail errmsg
                )
            )



-- extracted from the rails console with: analyte.json_qc.to_json


exampleData : String
exampleData =
    "{\"vals\":{\"stats\":{\"nominal\":5.0,\"mean\":4.772857142857142,\"deviation\":0.15157003724508414},\"qcresults\":[{\"id\":31008274,\"c\":4.718,\"d\":\"2018-04-12T13:34:51.000Z\"},{\"id\":31008266,\"c\":4.71,\"d\":\"2018-04-12T12:40:35.000Z\"},{\"id\":31008260,\"c\":4.744,\"d\":\"2018-04-12T12:23:19.000Z\"},{\"id\":30984474,\"c\":4.552,\"d\":\"2018-04-11T12:07:22.000Z\"},{\"id\":30984465,\"c\":4.757,\"d\":\"2018-04-11T11:04:41.000Z\"},{\"id\":30953440,\"c\":4.903,\"d\":\"2018-04-10T13:30:06.000Z\"},{\"id\":30953428,\"c\":5.026,\"d\":\"2018-04-10T12:28:07.000Z\"}]}}"


extract : String -> Maybe MyData
extract j =
    let
        a =
            decodeString decodeMyData j
    in
        case a of
            Ok z ->
                Just z

            Err _ ->
                Nothing


destruktor : String -> List Cid
destruktor s =
    let
        d =
            extract s
    in
        case d of
            Nothing ->
                []

            Just a ->
                a.vals.qcresults


type Msg
    = TryApi
    | Hover (Maybe Cid)
    | ShowData
    | NoOp
    | ItemsResult (Result Http.Error String)


newStyle : String -> String
newStyle s =
    (case s of
        "block" ->
            "none"

        "none" ->
            "block"

        _ ->
            "block"
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Hover hovered ->
            ( { model | hovered = hovered }, Cmd.none )

        ShowData ->
            ( { model
                | dataDisplay =
                    newStyle model.dataDisplay
              }
            , Cmd.none
            )

        TryApi ->
            let
                cmd =
                    Http.send ItemsResult <|
                        apiGet 18326
            in
                ( model, cmd )

        ItemsResult (Ok json_data) ->
            ( { model
                | stats = destruktor json_data
              }
            , Cmd.none
            )

        ItemsResult (Err err) ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


currentAnalyte : { c | flags : { b | analyteid : a } } -> a
currentAnalyte model =
    model.flags.analyteid


apiGet : a -> Http.Request String
apiGet id =
    let
        _ =
            Debug.log "running apiGet" 1

        host =
            "http://localhost:3000"

        path =
            "/chart_points/show/"
    in
        Http.getString (host ++ path ++ (toString id) ++ ".json")



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div
            [ (style [ ( "backgroundColor", "#fe0" ) ]) ]
            [ div
                [ (style
                    [ ( "border", "solid red 1px" )
                    , ( "width", "400px" )
                    , ( "margin", "1em auto" )
                    ]
                  )
                , onClick (TryApi)
                ]
                [ text "click here to load chart remotely" ]
            ]
        , div [] [ text (toString model.hovered) ]
        , div
            [ (style
                [ ( "border", "solid green 1px" )
                , ( "width", "400px" )
                , ( "margin", "1em auto" )
                ]
              )
            , onClick (ShowData)
            ]
            [ text "click here to toggle data" ]
        , div
            [ (id "data-list")
            , (style
                [ ( "display", model.dataDisplay )
                , ( "backgroundColor", "#44ffaa" )
                , ( "margin", "1em" )
                ]
              )
            ]
            [ toHtmlList model
            ]
        , div
            [ (style
                [ ( "border", "solid rgb(182, 235, 205) 1px" )
                , ( "width", "715px" )
                , ( "margin", "1em auto" )
                , ( "padding", "auto" )
                ]
              )
            ]
            [ showChart model ]
        ]


showChart model =
    LineChart.viewCustom (chartConfig model)
        [ LineChart.line Colors.purple
            Dots.square
            "QC"
            (List.map
                (\d ->
                    { x = (ISO8601.toTime d.d)
                    , y = d.c
                    }
                )
                model.stats
            )
        ]



-- there seems to be a problem with custom junk documentation
-- need to review on Monday


chartConfig model =
    { y = Axis.default 400 "Concentration" .y
    , x = Axis.time 750 "Date" .x
    , container = Container.spaced "line-chart-1" 60 140 60 120
    , interpolation = Interpolation.default
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.hoverOne Hover
    , junk =
        Junk.default

    -- Junk.hoverOne model.hovered
    --     [ ( "conc", toString << .c )
    --     , ( "date", toString << .d )
    --     ]
    , grid = Grid.default
    , area = Area.default
    , line = Line.default
    , dots = Dots.default
    }


toHtmlList : { c | stats : List { b | c : a, d : ISO8601.Time } } -> Html msg
toHtmlList model =
    ul [] (List.map to_i model.stats)


to_i : { b | c : a, d : ISO8601.Time } -> Html msg
to_i d =
    li [] [ text (relevantData d) ]


relevantData : { b | c : a, d : ISO8601.Time } -> String
relevantData d =
    "data: "
        ++ (toString d.c)
        ++ " "
        ++ (ISO8601.toString d.d)
        ++ " "
        ++ (toString (ISO8601.toTime d.d))



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
