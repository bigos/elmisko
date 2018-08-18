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
import Time
import Date
import Date.Format exposing (format)
import Random


-- MODEL


type alias Model =
    { data : Data
    , hinted : List Datum
    }


type alias Data =
    { nora : List Datum
    , noah : List Datum
    , nina : List Datum
    }


type alias Datum =
    { time : Time.Time
    , velocity : Float
    }



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { data = Data [] [] []
      , hinted = []
      }
    , generateData
    )


generateData : Cmd Msg
generateData =
    let
        genNumbers =
            Random.list 40 (Random.float 5 20)
    in
        Random.map3 (,,) genNumbers genNumbers genNumbers
            |> Random.generate RecieveData



-- API


setData : ( List Float, List Float, List Float ) -> Model -> Model
setData ( n1, n2, n3 ) model =
    { model | data = Data (toData n1) (toData n2) (toData n3) }


toData : List Float -> List Datum
toData numbers =
    let
        toDatum index velocity =
            Datum (indexToTime index) velocity
    in
        List.indexedMap toDatum numbers


indexToTime : Int -> Time.Time
indexToTime index =
    Time.hour
        * 24
        * 356
        * 45
        + -- 45 years
          Time.hour
        * 24
        * 30
        + -- a month
          Time.hour
        * 1
        * toFloat index



-- hours from first datum


setHint : List Datum -> Model -> Model
setHint hinted model =
    { model | hinted = hinted }



-- UPDATE


type Msg
    = RecieveData ( List Float, List Float, List Float )
    | Hint (List Datum)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecieveData numbers ->
            model
                |> setData numbers
                |> addCmd Cmd.none

        Hint points ->
            model
                |> setHint points
                |> addCmd Cmd.none


addCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
addCmd cmd model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ LineChart.viewCustom (chartConfig model)
            [ LineChart.line Colors.pink Dots.diamond "Nora" model.data.nora
            , LineChart.line Colors.cyan Dots.circle "Noah" model.data.noah
            , LineChart.line Colors.blue Dots.triangle "Nina" model.data.nina
            ]
        ]



-- CHART CONFIG


chartConfig : Model -> LineChart.Config Datum Msg
chartConfig model =
    { y = Axis.default 450 "velocity" .velocity
    , x = Axis.time 1270 "time" .time
    , container = containerConfig
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.hoverMany Hint
    , junk = Junk.hoverMany model.hinted formatX formatY
    , grid = Grid.dots 1 Colors.gray
    , area = Area.stacked 0.5
    , line = Line.default
    , dots = Dots.custom (Dots.empty 5 1)
    }


containerConfig : Container.Config Msg
containerConfig =
    Container.custom
        { attributesHtml = []
        , attributesSvg = []
        , size = Container.relative
        , margin = Container.Margin 30 100 30 70
        , id = "line-chart-area"
        }


formatX : Datum -> String
formatX datum =
    Date.Format.format "%e. %b, %Y" (Date.fromTime datum.time)


formatY : Datum -> String
formatY datum =
    toString (round100 datum.velocity) ++ " m/s"



-- UTILS


round100 : Float -> Float
round100 float =
    toFloat (round (float * 100)) / 100



-- PROGRAM


main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
